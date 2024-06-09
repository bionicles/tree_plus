import { Injectable } from '@angular/core';

import * as geolib from 'geolib';
import { openDB } from 'idb';

import {
    SearchCriteria,
    PromoTableName,
    extract_id,
    verify_matching,
} from '../models';

interface DBCommand<T = any> {
    db_name?: string;
    table_name: PromoTableName;
    model?: T;
    criteria?: SearchCriteria;
    where?: { [key: string]: string | number };
    key?: string | number;
}


@Injectable({
    providedIn: 'root',
})
export class IndexedDbService {
    private dbs: { [key: string]: any } = {};

    constructor() { }

    async create_connection({ db_name = 'client_db', table_name }: DBCommand) {
        if (!this.dbs[db_name]) {
            this.dbs[db_name] = await openDB(db_name, 1, {
                upgrade(db) {
                    if (!db.objectStoreNames.contains(table_name)) {
                        db.createObjectStore(table_name);
                    }
                },
            });
        }

        return this.dbs[db_name];
    }

    async create_model({ db_name, table_name, model }: DBCommand) {
        if (!model)
            throw new Error('Model is required for IndexedDbService.create');
        verify_matching({ table_name, model });
        const key = extract_id({ table_name, model });
        return (await this.create_connection({ db_name, table_name })).put(
            table_name,
            model,
            key
        );
    }

    async read_key({ db_name, table_name, key }: DBCommand) {
        if (!key) {
            throw new Error('Key is required for IndexedDbService.read');
        }
        return (await this.create_connection({ db_name, table_name })).get(
            table_name,
            key
        );
    }

    async update_model({ db_name, table_name, model }: DBCommand) {
        if (!model) {
            throw new Error('Key and model are required for IndexedDbService.update');
        }
        verify_matching({ table_name, model });
        const key = extract_id({ table_name, model });
        if (!(await this.read_key({ key, db_name, table_name }))) {
            throw new Error(`Cannot update, item with key ${key} not found`);
        }
        return this.create_model({ key, model, db_name, table_name });
    }

    async delete_key({ db_name, table_name, key }: DBCommand) {
        if (!key) {
            throw new Error('Key is required for IndexedDbService.delete');
        }
        return (await this.create_connection({ db_name, table_name })).delete(
            table_name,
            key
        );
    }

    async list_table({
        db_name,
        table_name,
        where,
    }: DBCommand & { where?: { [key: string]: string | number } }) {
        const allObjects = await (
            await this.create_connection({ db_name, table_name })
        ).getAll(table_name);

        // If no filter is provided, return all objects.
        if (!where) return allObjects;

        // Apply filters.
        return allObjects.filter((object: any) =>
            Object.entries(where).every(
                ([key, value]) => object.hasOwnProperty(key) && object[key] === value
            )
        );
    }

    async search_table(criteria: SearchCriteria) {
        const { db_name, table_name } = criteria;
        const objects = await this.list_table({
            db_name,
            table_name,
        });
        const lowerCaseQuery = criteria.query ? criteria.query.toLowerCase() : null;

        return objects.filter((object: any) => {
            const matchesQuery = lowerCaseQuery
                ? ['name', 'performer', 'lineup', 'rewards'].some(
                    (key) =>
                        object[key] &&
                        object[key].toString().toLowerCase().includes(lowerCaseQuery)
                )
                : true;

            const isWithinTimeRange =
                (criteria.start_time ? object.date >= criteria.start_time : true) &&
                (criteria.end_time ? object.date <= criteria.end_time : true);

            const isWithinRadius =
                criteria.latitude &&
                    criteria.longitude &&
                    criteria.radius_miles &&
                    object.location
                    ? geolib.isPointWithinRadius(
                        {
                            latitude: object.location.latitude,
                            longitude: object.location.longitude,
                        },
                        { latitude: criteria.latitude, longitude: criteria.longitude },
                        criteria.radius_miles * 1609.34 // convert miles to meters
                    )
                    : true;

            return matchesQuery && isWithinTimeRange && isWithinRadius;
        });
    }
}
