redMemoize = {(0, True): 0, (0, False): 1}
def redTiles(n, needTile=True):
    if n < 0:
        return 0
    if (n, needTile) not in redMemoize:
        redMemoize[(n, needTile)] = redTiles(n-1, needTile) + redTiles(n-2, False)
    return redMemoize[(n, needTile)]

greenMemoize = {(0, True): 0, (0, False): 1}
def greenTiles(n, needTile=True):
    if n < 0:
        return 0
    if (n, needTile) not in greenMemoize:
        greenMemoize[(n, needTile)] = greenTiles(n-1, needTile) + greenTiles(n-3, False)
    return greenMemoize[(n, needTile)]

blueMemoize = {(0, True): 0, (0, False): 1}
def blueTiles(n, needTile=True):
    if n < 0:
        return 0
    if (n, needTile) not in blueMemoize:
        blueMemoize[(n, needTile)] = blueTiles(n-1, needTile) + blueTiles(n-4, False)
    return blueMemoize[(n, needTile)]

print(redTiles(50) + greenTiles(50) + blueTiles(50))
