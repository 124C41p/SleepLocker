import fs from 'fs';
import _ from 'lodash';

export const characterClasses: CharacterClass[] = JSON.parse(fs.readFileSync('config/classes.json').toString());
export const allLoot: Dungeon[] = JSON.parse(fs.readFileSync('config/loot.json').toString());
export const uniqueLoot: Dungeon[] = allLoot.map(ins => getUniqueLoot(ins));

export interface CharacterClass {
    name: string;
    specializations: string[];
}

export interface Dungeon {
    name: string;
    locations: LootLocation[];
}

export interface LootLocation {
    location: string;
    loot: string[];
}

interface Item {
    name: string;
    location: string;
}

function getUniqueLoot(dungeon: Dungeon): Dungeon {
    let allItems: Item[] = _.flatten(dungeon.locations.map(loc => loc.loot.map(itemName => ({ location: loc.location, name: itemName }))));
    let locationGroups = _.mapValues(_.groupBy(allItems, 'name'), itemList => itemList.map(item => item.location));
    let uniqueItems: Item[] = _.map(locationGroups, (group, itemName) => ({ name: itemName, location: group.length == 1 ? group[0] : 'Geteilter Loot' }))
    let itemGroups = _.groupBy(uniqueItems, 'location');
    let lootLocations = _.map(itemGroups, (items, location) => ({ location: location, loot: items.map(item => item.name) }))
    return { name: dungeon.name, locations: lootLocations };
}