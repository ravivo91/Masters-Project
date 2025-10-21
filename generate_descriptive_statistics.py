import csv
import math
import statistics
import xml.etree.ElementTree as ET
import zipfile
from collections import OrderedDict
from pathlib import Path

ROOT = Path(__file__).resolve().parent


def read_xlsx(path):
    path = Path(path)
    with zipfile.ZipFile(path) as z:
        shared = []
        if 'xl/sharedStrings.xml' in z.namelist():
            root = ET.fromstring(z.read('xl/sharedStrings.xml'))
            ns_uri = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
            for si in root.findall('{%s}si' % ns_uri):
                text = ''.join(node.text or '' for node in si.findall('.//{%s}t' % ns_uri))
                shared.append(text)
        sheet = ET.fromstring(z.read('xl/worksheets/sheet1.xml'))
        ns_uri = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
        rows = []
        for row in sheet.findall('{%s}sheetData/{%s}row' % (ns_uri, ns_uri)):
            row_data = OrderedDict()
            for c in row.findall('{%s}c' % ns_uri):
                ref = c.get('r')
                if not ref:
                    continue
                col = ''.join(filter(str.isalpha, ref))
                t = c.get('t')
                v = c.find('{%s}v' % ns_uri)
                if v is None:
                    value = ''
                else:
                    text = v.text or ''
                    if t == 's':
                        value = shared[int(text)] if text else ''
                    else:
                        value = text
                row_data[col] = value
            rows.append(row_data)
        if not rows:
            return []
        header_row = rows[0]
        cols_sorted = sorted(header_row.keys(), key=lambda x: (len(x), x))
        headers = [header_row[col] for col in cols_sorted]
        data = []
        for row in rows[1:]:
            entry = OrderedDict()
            for col, name in zip(cols_sorted, headers):
                entry[name] = row.get(col, '')
            data.append(entry)
        return data


def read_csv(path):
    path = Path(path)
    with path.open(newline='') as f:
        reader = csv.DictReader(f)
        return [OrderedDict(row) for row in reader]


def to_float(value):
    if value is None:
        return None
    if isinstance(value, (int, float)):
        if math.isnan(value):
            return None
        return float(value)
    text = str(value).strip()
    if not text:
        return None
    try:
        return float(text)
    except ValueError:
        return None


def compute_stats(rows, ignore_columns=None):
    if not rows:
        return OrderedDict()
    if ignore_columns is None:
        ignore_columns = set()
    else:
        ignore_columns = set(ignore_columns)
    columns = list(rows[0].keys())
    stats = OrderedDict()
    for column in columns:
        if column in ignore_columns:
            continue
        values = []
        for row in rows:
            num = to_float(row.get(column))
            if num is not None:
                values.append(num)
        if not values:
            continue
        mean = sum(values) / len(values)
        std = statistics.stdev(values) if len(values) >= 2 else float('nan')
        stats[column] = {
            'count': len(values),
            'mean': mean,
            'std': std,
            'min': min(values),
            'max': max(values),
        }
    return stats


def format_float(value):
    if value is None or (isinstance(value, float) and math.isnan(value)):
        return 'N/A'
    value = float(value)
    if value == 0:
        return '0.0000'
    abs_val = abs(value)
    if abs_val >= 1000:
        return f"{value:,.1f}"
    if abs_val >= 10:
        return f"{value:.2f}"
    if abs_val >= 1:
        return f"{value:.3f}"
    if abs_val >= 0.1:
        return f"{value:.4f}"
    if abs_val >= 0.01:
        return f"{value:.5f}"
    return f"{value:.2e}"


def format_table(stats):
    lines = ["| Measure | Count | Mean | Std Dev | Min | Max |", "| --- | ---: | ---: | ---: | ---: | ---: |"]
    for measure, values in stats.items():
        lines.append(
            f"| {measure} | {values['count']} | {format_float(values['mean'])} | "
            f"{format_float(values['std'])} | {format_float(values['min'])} | {format_float(values['max'])} |"
        )
    return "\n".join(lines)


def subject_count(rows, subject_key):
    return len({row.get(subject_key) for row in rows if row.get(subject_key)})


def main():
    flash = read_xlsx(ROOT / 'AllSubjectsResultsFlash.xlsx')
    flicker = read_xlsx(ROOT / 'AllSubjectsResultsFlicker.xlsx')
    psych = read_csv(ROOT / 'analysis_psychophysical_data.csv')

    flash_stats = compute_stats(flash, ignore_columns={'SubjectID'})
    flicker_stats = compute_stats(flicker, ignore_columns={'SubjectID'})
    psych_stats = compute_stats(psych, ignore_columns={'subject'})

    sections = []
    sections.append(
        "# Descriptive Statistics for ERG/VEP and Psychophysics\n\n"
        "This summary provides descriptive statistics for the flash ERG dataset, the flicker ERG/VEP dataset, "
        "and psychophysical measurements. Counts reflect the number of non-missing observations per measure.\n"
    )

    sections.append(
        f"## Flash ERG Timing and Amplitude Metrics\n\n"
        f"Subjects included: {subject_count(flash, 'SubjectID')}\n\n"
        f"{format_table(flash_stats)}\n"
    )

    sections.append(
        f"## Flicker ERG/VEP Amplitude, Phase, and SNR Metrics\n\n"
        f"Subjects included: {subject_count(flicker, 'SubjectID')}\n\n"
        f"{format_table(flicker_stats)}\n"
    )

    sections.append(
        f"## Psychophysical Performance Metrics\n\n"
        f"Subjects included: {subject_count(psych, 'subject')}\n\n"
        f"{format_table(psych_stats)}\n"
    )

    output_path = ROOT / 'Descriptive_Statistics_ERG_VEP_Psychophysics.md'
    output_path.write_text("\n".join(sections))


if __name__ == '__main__':
    main()
