from hypothesis import given, example, assume
import hypothesis.strategies as st

def cycleIt(it):
    try:
        n = 1
        while True:
            x = next(it)
            for _ in range(n):
                y = next(it)
                if x == y:
                    return True
            n *= 2
    except StopIteration:
        return False

import itertools as it
def findCycle(it):
    """Returns (intro length, cycle length) of an iterator.
    Gives cycle length as 0, for finite iterator.

    Assumes
    """
    i = -1
    for i, x in enumerate(it):
        if i & i + 1:
            if x == y:
                return j + 1, i - j
        else:
            j, y = i, x
    else:
        return i + 1, 0

@st.composite
def looping(draw):
    l = draw(st.lists(st.integers(), unique=True))
    n = draw(st.integers(min_value=0, max_value=len(l)))
    return l[:n], l[n:]

def mod(a, b):
    if 0 == b:
        return a
    else:
        return a % b

@given(looping=looping())
def asExpected(looping):
    intro, loop = looping
    iter = it.chain(intro, it.cycle(loop))

    introA, loopA = findCycle(iter)
    assert introA >= len(intro)
    assert len(loop) == loopA, (len(loop), loopA)

@given(iter=st.iterables(st.integers(), unique=True))
def worksWithoutIndex(iter):
    findCycle(iter)


@given(iter=st.iterables(st.integers()))
def worksWithoutIndex(iter):
    findCycle(iter)


asExpected()
worksWithoutIndex()
