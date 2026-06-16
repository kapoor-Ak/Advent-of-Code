import requests
import os


def get_session_cookie(filepath="cookie_monster.txt"):
    """Reads the session cookie from a local text file."""
    if not os.path.exists(filepath):
        raise FileNotFoundError(
            f"Could not find '{filepath}'. Please create it and paste your session cookie inside."
        )
    
    with open(filepath, "r") as f:
        # .strip() removes any accidental invisible spaces or newlines
        return f.read().strip()




def get_aoc(day, year=2025):
    session_cookie = get_session_cookie()
    # Validation logic
    if day is None:
        raise ValueError("Usage: get_aoc(day, year)")
    
    day = int(day)
    year = int(year)
    
    if not (1 <= day <= 25):
        raise ValueError("Day must be between 1 and 25")
    if year < 2015:
        raise ValueError("Advent of Code started in 2015")
    
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    
    # Headers and Cookies
    headers = {
        "User-Agent": "get_aoc Python script (https://adventofcode.com)"
    }
    cookies = {
        "session": session_cookie
    }
    
    try:
        response = requests.get(url, headers=headers, cookies=cookies)
        
        # Check if the request was successful
        if response.status_code != 200:
            raise Exception(
                f"Failed to fetch input: HTTP {response.status_code}. "
                "Make sure the session cookie is valid and the puzzle is unlocked."
            )
            
        return response.text
        
    except requests.exceptions.RequestException as e:
        raise Exception(f"Request failed: {e}")

# Example usage:
# The `if __name__ == "__main__"` guard means this block ONLY runs when you
# execute `python worker.py` directly. When another file does `import worker`,
# this block is SKIPPED -- so importing no longer triggers a surprise network
# call. (In R there's no real equivalent; sourcing a script always runs everything.)
if __name__ == "__main__":
    print(get_aoc(7, 2025))
