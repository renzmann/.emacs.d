import argparse
import time


DEFAULT_LENGTH = 100
DEFAULT_SLEEP = 0.3


parser = argparse.ArgumentParser()
parser.add_argument("--straight", action="store_true")
parser.add_argument("--length", type=int)
parser.add_argument("--sleep", type=float)
parser.add_argument("--no-flush", action="store_true")
args = parser.parse_args()


def cycle(
    length: int = DEFAULT_LENGTH, sleep: float = DEFAULT_SLEEP, flush: bool = True
) -> None:
    """Print `'Hello'` with a rotating number of `'!'` on the same line."""
    for i in range(1, length + 1):
        if i > 1:
            print("\033[K", end="")  # Clear to the end of line
        print("Hello" + "!" * (i % 5), end="\r", flush=flush)
        time.sleep(sleep)


def straight(
    length: int = DEFAULT_LENGTH, sleep: float = DEFAULT_SLEEP, flush: bool = True
) -> None:
    """Print `'Hello!'` a fixed number of times in a row, optionally flushing each time."""
    for _ in range(length):
        print("Hello!", flush=flush)
        time.sleep(sleep)


if __name__ == "__main__":
    length = DEFAULT_LENGTH if args.length is None else args.length
    sleep = DEFAULT_SLEEP if args.sleep is None else args.sleep
    flush = not args.no_flush
    prog = straight if args.straight else cycle

    try:
        prog(length=length, sleep=sleep, flush=flush)
    except KeyboardInterrupt:
        print("\nGoodbye!", flush=True)
