import { Database } from 'sqlite3';


/*
Modes
0 - not started yet
1 - receiving softlocks
2 - listing softlocks
3 - done
*/

export interface UserData {
    userName: string;
    class: string;
    specialization: string;
    prio1: string|null;
    prio2: string|null;
}

export interface Raid {
    id: number;
    name: string;
    instance: string;
    mode: number;
    date: string;
}

export class SoftlockRegistrationError extends Error {

}

export class SoftlockCapacityReachedError extends Error {

}

export class SoftlockCancellationError extends Error {

}

export class RaidDatabase {
    private _db = new Database('database.sqlite');

    async initialize() {
        return new Promise((resolve, reject) => {
            this._db.serialize(() => {
                this._db.exec('CREATE TABLE IF NOT EXISTS raids(raid_id INTEGER PRIMARY KEY AUTOINCREMENT, name STRING NOT NULL, instance STRING NOT NULL, mode INTEGER DEFAULT 0, date STRING)', err => {
                    if(err) return reject(err);
                });
                this._db.exec('CREATE TABLE IF NOT EXISTS locks(raid_id INTEGER, user_name STRING NOT NULL, class STRING NOT NULL, specialization STRING NOT NULL, prio1 STRING, prio2 STRING, editable INTEGER DEFAULT 1, PRIMARY KEY(raid_id, user_name))', err => {
                    if(err) return reject(err);
                });
                resolve();
            });
        });
    }

    async currentRaidId(): Promise<number|null> {
        return new Promise((resolve, reject) => {
            this._db.get('SELECT MAX(raid_id) as no FROM raids WHERE mode < 3', (err, row) => {
                if(err) return reject(err);
                resolve(row.no);
            });
        });
    }

    async getUserData(raidID: number, userName: string): Promise<UserData|null> {
        return new Promise((resolve, reject) => {
            this._db.get('SELECT class, specialization, prio1, prio2, editable FROM locks WHERE raid_id = ? AND user_name = ?', [raidID, userName], (err, row) => {
                if(err) return reject(err);
                if(!row || !row.editable) return resolve(null);
                resolve({ userName: userName, class: row.class, specialization: row.specialization, prio1: row.prio1, prio2: row.prio2 });
            });
        });
    }

    async setUserLocks(raidID: number, userName: string, userClass: string, specialization: string, prio1?: string, prio2?: string) {
        return new Promise((resolve, reject) => {
            this._db.get('SELECT COUNT(*) as no FROM locks WHERE raid_id = ?', [raidID], (err, row) => {
                if(err) return reject(err);
                if(row.no > 80) return reject(new SoftlockCapacityReachedError());
    
                this._db.run('INSERT INTO locks(raid_id, user_name, class, specialization, prio1, prio2) VALUES(?,?,?,?,?,?)', [raidID, userName, userClass, specialization, prio1, prio2], err => {
                    if(err) return reject(new SoftlockRegistrationError());
                    resolve();
                });
            });
        });
    }
    
    async getRaid(raidID: number): Promise<Raid> {
        return new Promise((resolve, reject) => {
            this._db.get('SELECT raid_id as id, name, instance, mode, date FROM raids WHERE raid_id = ?', [raidID], (err, row) => {
                if(err) return reject(err);
                resolve(row as Raid);
            });
        });
    }

    async listRaidLocks(raidID: number): Promise<UserData[]> {
        return new Promise((resolve, reject) => {
            this._db.all('SELECT user_name as name, class, specialization, prio1, prio2 FROM locks WHERE raid_id = ?', [raidID], (err, rows) => {
                if(err) return reject(err);
                resolve(rows);
            });
        });
    }
    
    async removeLock(raidID: number, userName: string) {
        return new Promise((resolve, reject) => {
            this._db.get('SELECT editable FROM locks WHERE raid_id = ? AND user_name = ?', [raidID, userName], (err, row) => {
                if(!row || !row.editable)
                    return reject(new SoftlockCancellationError());
                this._db.run('DELETE FROM locks WHERE raid_id = ? AND user_name = ?', [raidID, userName], err => {
                    if(err) return reject(new SoftlockCancellationError());
                    resolve();
                });
            });
        });
    }

    async close() {
        return new Promise((resolve, reject) => {
            this._db.close(err => {
                if(err)
                    return reject(err);
                resolve();
            });
        });
    }

}


