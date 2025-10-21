#!/usr/bin/env python3
"""Generate per-variable histograms for ERG/VEP and psychophysical datasets.

This script reads the flash ERG, flicker ERG/VEP, and psychophysical datasets
from Excel/CSV sources and produces a histogram for every numeric variable in
separate SVG files. It avoids third-party dependencies by implementing a small
Excel reader and SVG plotting helpers using only the Python standard library.
"""

from __future__ import annotations

import csv
import math
import os
import sys
import zipfile
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Tuple
from xml.etree import ElementTree

MAIN_NS = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
REL_NS_PACKAGE = "http://schemas.openxmlformats.org/package/2006/relationships"
REL_NS_DOCUMENT = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"


def column_letters_to_index(letters: str) -> int:
    """Convert Excel column letters (e.g., "A", "AB") to a zero-based index."""

    result = 0
    for ch in letters:
        if not ch.isalpha():
            raise ValueError(f"Invalid column letter {letters!r}")
        result = result * 26 + (ord(ch.upper()) - ord("A") + 1)
    return result - 1


def load_shared_strings(zip_file: zipfile.ZipFile) -> List[str]:
    """Return the shared string table if present."""

    try:
        data = zip_file.read("xl/sharedStrings.xml")
    except KeyError:
        return []

    root = ElementTree.fromstring(data)
    ns = {"main": MAIN_NS}
    strings: List[str] = []
    for si in root.findall("main:si", ns):
        text_parts: List[str] = []
        for t in si.findall('.//main:t', ns):
            if t.text:
                text_parts.append(t.text)
        strings.append("".join(text_parts))
    return strings


def discover_sheets(zip_file: zipfile.ZipFile) -> List[Tuple[str, str]]:
    """Return [(sheet_name, internal_path)] ordered as in the workbook."""

    try:
        workbook_xml = zip_file.read("xl/workbook.xml")
        rels_xml = zip_file.read("xl/_rels/workbook.xml.rels")
    except KeyError as exc:
        raise ValueError("Not a valid Excel workbook") from exc

    ns = {"main": MAIN_NS, "rel": REL_NS_DOCUMENT}
    workbook_root = ElementTree.fromstring(workbook_xml)
    sheets = workbook_root.findall("main:sheets/main:sheet", ns)

    rel_root = ElementTree.fromstring(rels_xml)
    rel_ns = {"rel": REL_NS_PACKAGE}
    rel_map: Dict[str, str] = {}
    for rel in rel_root.findall("rel:Relationship", rel_ns):
        rel_id = rel.attrib.get("Id")
        target = rel.attrib.get("Target")
        if rel_id and target:
            if not target.startswith("/"):
                rel_map[rel_id] = f"xl/{target}"
            else:
                rel_map[rel_id] = target.lstrip("/")

    sheet_info: List[Tuple[str, str]] = []
    for sheet in sheets:
        name = sheet.attrib.get("name", "Sheet1")
        rel_id = sheet.attrib.get(f"{{{REL_NS_DOCUMENT}}}id")
        if not rel_id:
            continue
        target = rel_map.get(rel_id)
        if not target:
            continue
        sheet_info.append((name, target))
    if not sheet_info:
        raise ValueError("Workbook does not contain readable sheets")
    return sheet_info


def parse_cell_value(cell, shared_strings: Sequence[str]) -> Optional[str]:
    """Return the raw value stored in an Excel cell as a string."""

    ns = {"main": MAIN_NS}
    cell_type = cell.attrib.get("t")
    if cell_type == "s":
        value_elem = cell.find("main:v", ns)
        if value_elem is None or value_elem.text is None:
            return None
        try:
            index = int(value_elem.text)
        except ValueError:
            return None
        if 0 <= index < len(shared_strings):
            return shared_strings[index]
        return None
    if cell_type == "inlineStr":
        text_parts: List[str] = []
        for t in cell.findall("main:is//main:t", ns):
            if t.text:
                text_parts.append(t.text)
        return "".join(text_parts) if text_parts else None
    if cell_type == "b":
        value_elem = cell.find("main:v", ns)
        if value_elem is None or value_elem.text is None:
            return None
        return "1" if value_elem.text.strip() == "1" else "0"

    value_elem = cell.find("main:v", ns)
    if value_elem is None:
        return None
    return value_elem.text


def read_xlsx(path: str, sheet_name: Optional[str] = None) -> List[Dict[str, Optional[str]]]:
    """Read an Excel sheet into a list of dictionaries using stdlib only."""

    with zipfile.ZipFile(path) as zf:
        shared_strings = load_shared_strings(zf)
        sheets = discover_sheets(zf)
        if sheet_name:
            target_sheet = next((target for name, target in sheets if name == sheet_name), None)
            if target_sheet is None:
                available = ", ".join(name for name, _ in sheets)
                raise ValueError(f"Sheet {sheet_name!r} not found in {path}. Available: {available}")
        else:
            target_sheet = sheets[0][1]

        sheet_xml = zf.read(target_sheet)

    ns = {"main": MAIN_NS}
    root = ElementTree.fromstring(sheet_xml)
    sheet_data = root.find("main:sheetData", ns)
    if sheet_data is None:
        return []

    rows_raw: List[Dict[int, Optional[str]]] = []
    max_col_index = -1
    for row in sheet_data.findall("main:row", ns):
        cells_map: Dict[int, Optional[str]] = {}
        for cell in row.findall("main:c", ns):
            ref = cell.attrib.get("r", "")
            col_letters = "".join(ch for ch in ref if ch.isalpha())
            if not col_letters:
                continue
            col_index = column_letters_to_index(col_letters)
            if col_index > max_col_index:
                max_col_index = col_index
            cells_map[col_index] = parse_cell_value(cell, shared_strings)
        rows_raw.append(cells_map)

    if max_col_index < 0:
        return []

    row_values: List[List[Optional[str]]] = []
    for cells_map in rows_raw:
        row_list = [cells_map.get(idx) for idx in range(max_col_index + 1)]
        row_values.append(row_list)

    if not row_values:
        return []

    header_row = row_values[0]
    headers: List[str] = []
    seen: Dict[str, int] = {}
    for idx, raw_value in enumerate(header_row):
        text = (raw_value or "").strip()
        if not text:
            text = f"Column{idx + 1}"
        count = seen.get(text, 0)
        if count:
            unique = f"{text}_{count + 1}"
        else:
            unique = text
        seen[text] = count + 1
        headers.append(unique)

    records: List[Dict[str, Optional[str]]] = []
    for row in row_values[1:]:
        if all((cell is None or (isinstance(cell, str) and not cell.strip())) for cell in row):
            continue
        record = {header: (row[idx] if idx < len(row) else None) for idx, header in enumerate(headers)}
        records.append(record)
    return records


def read_csv(path: str) -> List[Dict[str, str]]:
    with open(path, newline="", encoding="utf-8") as fh:
        reader = csv.DictReader(fh)
        return [row for row in reader]


def coerce_float(value: Optional[str]) -> Optional[float]:
    if value is None:
        return None
    if isinstance(value, (int, float)):
        return float(value)
    text = str(value).strip()
    if not text:
        return None
    text = text.replace(",", "")
    try:
        return float(text)
    except ValueError:
        return None


def sanitise_filename(name: str) -> str:
    safe = [ch if ch.isalnum() or ch in ("-", "_") else "_" for ch in name]
    cleaned = "".join(safe).strip("_")
    return cleaned or "variable"


def format_tick(value: float) -> str:
    abs_val = abs(value)
    if abs_val >= 1000:
        return f"{value:,.0f}"
    if abs_val >= 100:
        return f"{value:,.1f}"
    if abs_val >= 1:
        return f"{value:.2f}"
    if abs_val >= 0.1:
        return f"{value:.3f}"
    return f"{value:.2e}"


def determine_bins(values: Sequence[float], desired_bins: int = 12) -> Tuple[List[float], List[int]]:
    if not values:
        return [0.0, 1.0], [0]
    finite_values = [v for v in values if math.isfinite(v)]
    if not finite_values:
        return [0.0, 1.0], [0]
    min_val = min(finite_values)
    max_val = max(finite_values)
    if math.isclose(min_val, max_val):
        min_val -= 0.5
        max_val += 0.5
    span = max_val - min_val
    if span <= 0:
        span = 1.0
    bin_count = max(3, min(desired_bins, len(finite_values)))
    bin_width = span / bin_count
    edges = [min_val + i * bin_width for i in range(bin_count + 1)]
    counts = [0] * bin_count
    for value in finite_values:
        if value == max_val:
            index = bin_count - 1
        else:
            index = int((value - min_val) / bin_width)
            index = max(0, min(index, bin_count - 1))
        counts[index] += 1
    return edges, counts


def svg_histogram(values: Sequence[float], title: str, output_path: str) -> None:
    edges, counts = determine_bins(values)
    max_count = max(counts) if counts else 0
    if max_count == 0:
        max_count = 1

    width, height = 900, 560
    margin_left, margin_right = 90, 30
    margin_top, margin_bottom = 70, 70
    plot_width = width - margin_left - margin_right
    plot_height = height - margin_top - margin_bottom

    bar_count = len(counts)
    bar_width = plot_width / bar_count if bar_count else plot_width

    svg_parts: List[str] = []
    svg_parts.append(
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">' 
    )
    svg_parts.append('<rect width="100%" height="100%" fill="#ffffff"/>')
    svg_parts.append(
        f'<text x="{width / 2:.1f}" y="{margin_top / 2:.1f}" text-anchor="middle" '
        f'font-size="24" font-family="Helvetica, Arial, sans-serif" fill="#111827">{title}</text>'
    )

    # Axes
    x_axis_y = margin_top + plot_height
    svg_parts.append(
        f'<line x1="{margin_left}" y1="{x_axis_y}" x2="{margin_left + plot_width}" '
        f'y2="{x_axis_y}" stroke="#1f2937" stroke-width="2"/>'
    )
    svg_parts.append(
        f'<line x1="{margin_left}" y1="{margin_top}" x2="{margin_left}" '
        f'y2="{x_axis_y}" stroke="#1f2937" stroke-width="2"/>'
    )

    # Bars
    if counts:
        scale = plot_height / max_count if max_count else 1.0
        for index, count in enumerate(counts):
            bar_height = count * scale
            x = margin_left + index * bar_width
            y = x_axis_y - bar_height
            svg_parts.append(
                f'<rect x="{x + 2:.2f}" y="{y:.2f}" width="{max(bar_width - 4, 1):.2f}" '
                f'height="{bar_height:.2f}" fill="#2563eb" opacity="0.85"/>'
            )

    # Y ticks
    y_ticks = 5
    for i in range(y_ticks + 1):
        proportion = i / y_ticks
        value = max_count * proportion
        y = x_axis_y - proportion * plot_height
        svg_parts.append(
            f'<line x1="{margin_left - 6}" y1="{y:.2f}" x2="{margin_left}" y2="{y:.2f}" '
            f'stroke="#1f2937" stroke-width="1"/>'
        )
        svg_parts.append(
            f'<text x="{margin_left - 10}" y="{y + 4:.2f}" text-anchor="end" font-size="14" '
            f'font-family="Helvetica, Arial, sans-serif" fill="#4b5563">{int(round(value))}</text>'
        )

    # X ticks
    min_edge, max_edge = edges[0], edges[-1]
    tick_marks = min(6, len(edges))
    if tick_marks <= 1:
        tick_marks = 2
    for i in range(tick_marks):
        proportion = i / (tick_marks - 1)
        value = min_edge + proportion * (max_edge - min_edge)
        x = margin_left + proportion * plot_width
        svg_parts.append(
            f'<line x1="{x:.2f}" y1="{x_axis_y}" x2="{x:.2f}" y2="{x_axis_y + 6}" '
            f'stroke="#1f2937" stroke-width="1"/>'
        )
        svg_parts.append(
            f'<text x="{x:.2f}" y="{x_axis_y + 24}" text-anchor="middle" font-size="14" '
            f'font-family="Helvetica, Arial, sans-serif" fill="#4b5563">{format_tick(value)}</text>'
        )

    svg_parts.append('</svg>')
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(svg_parts))


def collect_numeric_columns(records: Sequence[Dict[str, Optional[str]]], exclude: Iterable[str] = ()) -> Dict[str, List[float]]:
    exclusion = {name.lower() for name in exclude}
    columns: Dict[str, List[float]] = {}
    for record in records:
        for key, value in record.items():
            if key is None:
                continue
            if key.lower() in exclusion:
                continue
            numeric = coerce_float(value)
            if numeric is None or math.isnan(numeric):
                continue
            columns.setdefault(key, []).append(numeric)
    return {key: vals for key, vals in columns.items() if len(vals) >= 1}


@dataclass
class DatasetConfig:
    label: str
    path: str
    kind: str  # 'excel' or 'csv'
    sheet: Optional[str] = None
    exclude: Tuple[str, ...] = ("subject", "subjectid")


def load_dataset(config: DatasetConfig) -> List[Dict[str, Optional[str]]]:
    if config.kind == "excel":
        return read_xlsx(config.path, config.sheet)
    if config.kind == "csv":
        return read_csv(config.path)
    raise ValueError(f"Unsupported dataset kind: {config.kind}")


def main(argv: Sequence[str]) -> int:
    datasets = [
        DatasetConfig(label="flash", path="AllSubjectsResultsFlash.xlsx", kind="excel"),
        DatasetConfig(label="flicker", path="AllSubjectsResultsFlicker.xlsx", kind="excel"),
        DatasetConfig(label="psychophysics", path="analysis_psychophysical_data.csv", kind="csv"),
    ]

    output_root = "figures"
    generated = []
    for dataset in datasets:
        try:
            records = load_dataset(dataset)
        except FileNotFoundError:
            print(f"Warning: {dataset.path} not found. Skipping {dataset.label} dataset.", file=sys.stderr)
            continue
        except Exception as exc:  # pylint: disable=broad-except
            print(f"Warning: could not load {dataset.path}: {exc}", file=sys.stderr)
            continue

        numeric_columns = collect_numeric_columns(records, exclude=dataset.exclude)
        if not numeric_columns:
            print(f"No numeric columns found for {dataset.label}.", file=sys.stderr)
            continue
        dataset_dir = os.path.join(output_root, dataset.label)
        os.makedirs(dataset_dir, exist_ok=True)
        for column_name, values in sorted(numeric_columns.items()):
            filename = f"{sanitise_filename(column_name)}.svg"
            output_path = os.path.join(dataset_dir, filename)
            title = f"{column_name} ({dataset.label.capitalize()})"
            svg_histogram(values, title, output_path)
            generated.append(output_path)
            print(f"Generated {output_path}")

    if not generated:
        print("No plots were generated.")
        return 1
    print(f"Created {len(generated)} plots in '{output_root}'.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
