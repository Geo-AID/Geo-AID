import subprocess
import os
import sys
from pathlib import Path
from typing import List, Tuple
import csv
import time

def run_cargo_test() -> None:
    command = "cargo test --verbose --workspace"
    run_command(command)


def run_cargo_fmt() -> None:
    command = "cargo fmt --all -- --check"
    run_command(command)


def run_cargo_clippy() -> None:
    command = "cargo clippy --all-features --all-targets -- -D warnings"
    run_command(command)


def main() -> None:

    parser = argparse.ArgumentParser()
    parser.add_argument(
        'engine',
        choices=[e.value for e in Engine],  # This will be ['glide', 'rage']
        nargs='?',  # Makes it optional
        default=Engine.GLIDE.value,  # Default to "glide"
        help="Choose an engine: 'glide' or 'rage'. Default is 'glide'."
    )

    # if len(sys.argv) == 1:
    #     engine = "glide"
    # else:
    #     engine = sys.argv[1]

    # First, we run all tests through Geo-AID to their respective directories.
    tests = []
    for file in os.scandir("tests"):
        if file.is_file() and file.name.endswith(".geo"):
            print(f"Rendering {file.name}")

            path = Path(file.path)
            output = os.path.join("reports", path.stem)
            tests.append(path.stem)

            os.makedirs(output, exist_ok=True)

            proc = subprocess.Popen([
                "cargo", "run", "--release", "--",
                file.path, "-o", os.path.join(output),
                "-l", os.path.join(output, "log.log"),
                "-f", "svg", "-e", engine
            ])
            proc.communicate()
            proc.wait()

    records = [
        ["Name", "Result", "Previous quality", "New quality", "Delta", "Previous time", "New time", "Delta"]
    ]
    total_quality_old = 0
    total_quality_new = 0

    total_time_old = 0
    total_time_new = 0

    for name in tests:
        dir = os.path.abspath(os.path.join("reports", name))

        if os.path.exists(os.path.join(dir, "log-pre.log")):
            with open(os.path.join(dir, "log-pre.log")) as fp:
                lines = fp.readlines()
                # print(lines)

                old_quality = float(lines[1].strip())
                old_time = float(lines[2].strip())
        else:
            old_quality = 0
            old_time = 0

        total_quality_old += old_quality
        total_time_old += old_time

        with open(os.path.join(dir, "log.log")) as fp:
            lines = fp.readlines()
            # print(lines)

            if lines[0].strip() == "0":
                new_result = "ok"
                new_quality = float(lines[1].strip())
                new_time = float(lines[2].strip())
                total_time_new += new_time
                total_quality_new += new_quality

                # Replace the old file with the new one
                with open(os.path.join(dir, "log-pre.log"), "w+") as fp2:
                    fp2.writelines(lines)
            else:
                new_result = "error"
                new_quality = 0
                new_time = 0
                total_time_new += old_time
                total_quality_new += old_quality

        records.append([name, new_result, old_quality, new_quality, new_quality - old_quality, old_time, new_time,
                        new_time - old_time])

    with open(os.path.join("reports", "report.csv"), "w+", newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(records)
        testcount = len(tests)
        writer.writerow([
            "average",
            "-",
            total_quality_old / testcount,
            total_quality_new / testcount,
            (total_quality_new - total_quality_old) / testcount,
            total_time_old / testcount,
            total_time_new / testcount,
            (total_time_new - total_time_old) / testcount
        ])


if __name__ == "__main__":
    main()
