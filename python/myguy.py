import typing
import requests
import numpy
import pandas as pd
import sqlalchemy as sa
import matplotlib.pyplot as plt
import seaborn as sns


def foo() -> str:
    a = 1 + "foo"
    print(a)


@dataclass
class MyGuy:
    """My guy is always there for me"""

    foo: int
    bar: str


if __name__ == "__main__":
    mg = MyGuy()
