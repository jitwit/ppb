import requests
import json
import argparse
import time
import curses

def get_lichess_stats(user):
    url = f"https://lichess.org/api/user/{user}"
    response = requests.get(url)
    data = json.loads(response.text)
    return data

def get_chesscom_stats(user):
    url = f"https://api.chess.com/pub/player/{user}/stats"
    headers = {'User-Agent': f'SteamCounter (user: {user})'}
    response = requests.get(url, headers=headers)
    data = json.loads(response.text)
    return data

def main(stdscr, user, platform, wins, draws, losses, total):
    games_played = wins + draws + losses
    games_won = wins
    games_lost = draws
    games_drawn = losses

    stdscr.nodelay(True)
    stdscr.clear()
    curses.start_color()
    curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLACK)
    curses.init_pair(2, curses.COLOR_GREEN, curses.COLOR_BLACK)
    curses.init_pair(3, curses.COLOR_RED, curses.COLOR_BLACK)
    curses.init_pair(4, curses.COLOR_YELLOW, curses.COLOR_BLACK)
    stdscr.bkgd(" ", curses.color_pair(1))

    while True:
        
        if stdscr.getch() == ord('q'):
            break
        stdscr.clear()
        
        if platform == "lichess":
            stats = get_lichess_stats(user)
            current_games_played = stats['count']['all']
            current_games_won = stats['count']['win']
            current_games_drawn = stats['count']['draw']
            current_games_lost = stats['count']['loss']
        elif platform == "chesscom":
            stats = get_chesscom_stats(user)
            current_games_won = stats['chess_bullet']['record']['win']
            current_games_lost = stats['chess_bullet']['record']['loss']
            current_games_drawn = stats['chess_bullet']['record']['draw']
            current_games_played = current_games_drawn + current_games_won + current_games_lost

        if games_played == wins + draws + losses:
            games_played = current_games_played - wins - draws - losses
            games_won = current_games_won - wins
            games_lost = current_games_lost - draws
            games_drawn = current_games_drawn - losses

        games_played_since_start = current_games_played - games_played
        games_won_since_start = current_games_won - games_won
        games_lost_since_start = current_games_lost - games_lost
        games_drawn_since_start = current_games_drawn - games_drawn

        stdscr.addstr(0, 0, f"Bullet Marathon by {user}")
        stdscr.addstr(2, 0, f"Games\t{games_played_since_start}/{total}")
        stdscr.addstr(3, 0, f"Wins\t{games_won_since_start}", curses.color_pair(2))
        stdscr.addstr(4, 0, f"Losses\t{games_lost_since_start}", curses.color_pair(3))
        stdscr.addstr(5, 0, f"Draws\t{games_drawn_since_start}", curses.color_pair(4))

        stdscr.addstr(10, 0, "")

        time.sleep(3)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Fetch bullet stats from Lichess or Chess.com and update a text file.")
    parser.add_argument("user", help="Username on Lichess or Chess.com")
    parser.add_argument("platform", choices=["lichess", "chesscom"],
                        help="Platform to fetch stats from ('lichess' or 'chesscom')")
    parser.add_argument("--wins"  , type=int, default=0, nargs="?", help="starting wins")
    parser.add_argument("--draws" , type=int, default=0, nargs="?", help="starting draws")
    parser.add_argument("--losses", type=int, default=0, nargs="?", help="starting losses")
    parser.add_argument("--total" , type=int, default=1000, nargs="?", help="total")
    args = parser.parse_args()

    curses.wrapper(main, args.user, args.platform, args.wins, args.draws, args.losses, args.total)
