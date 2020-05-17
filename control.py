import sqlite3
conn = sqlite3.connect('database.sqlite')
from datetime import datetime

def active_raid_no():
    cur = conn.cursor()
    try:
        return cur.execute('SELECT MAX(raid_id) FROM raids WHERE mode < 3').fetchone()[0]
    except:
        return None
    finally:
        cur.close()

def add_raid(name, dungeon):
    cur = conn.cursor()
    cur.execute('INSERT INTO raids(name, dungeon, date) VALUES(?,?,?)', [name, dungeon, str(datetime.now().date())])
    cur.close()
    conn.commit()

def set_mode(raid_id, mode):
    cur = conn.cursor()
    cur.execute('UPDATE raids SET mode=? WHERE raid_id = ?', [mode, raid_id])
    cur.close()
    conn.commit()

def start():
    set_mode(active_raid_no(), 1)

def show():
    set_mode(active_raid_no(), 2)
    
def close():
    set_mode(active_raid_no(), 3)

def get_locks(raid_id = None):
    if raid_id is None:
        raid_id = active_raid_no()
    cur = conn.cursor()
    locks = cur.execute('SELECT user_name, prio1, prio2 FROM locks WHERE raid_id = ?', [raid_id]).fetchall()
    cur.close()
    return { name: (prio1, prio2) for name, prio1, prio2 in locks}

def set_lock(name, prio1, prio2, raid_id = None):
    if raid_id is None:
        raid_id = active_raid_no()
    cur = conn.cursor()
    try:
        cur.execute('INSERT INTO locks(raid_id, user_name, prio1, prio2, editable) VALUES (?, ?, ?, ?, 0)', [raid_id, name, prio1, prio2])
    except sqlite3.IntegrityError:
        cur.execute('UPDATE locks SET prio1=?, prio2=?, editable=0 WHERE raid_id=? AND user_name=?', [prio1, prio2, raid_id, name])
    cur.close()
    conn.commit()

def remove_lock(name, raid_id = None):
    if raid_id is None:
        raid_id = active_raid_no()
    cur = conn.cursor()
    cur.execute('DELETE FROM locks WHERE raid_id = ? AND user_name = ?', [raid_id, name])
    cur.close()
    conn.commit()
    