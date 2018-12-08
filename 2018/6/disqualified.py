disqualified = set()
  for v in grid[0]:
      disqualified.add(v)
  for v in grid[-1]:
      disqualified.add(v)
  for row in grid:
      disqualified.add(row[0])
  for row in grid:
      disqualified.add(row[-1])
