import chess
import sys

b = chess.Board()
result = []

for move in sys.argv[1:]:
    result.append(b.san(m := chess.Move.from_uci(move)))
    b.push(m)

print(" ".join(result))
