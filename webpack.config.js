'use strict';

const nodeExternals = require('webpack-node-externals');

const backConfig = (env = {}) => ({
    entry: ['./src/backend/main.ts'],
    output: {
        filename: 'SleepLockerServer.js',
        path: __dirname + '/dist/backend'
    },
    mode: env.development ? 'development' : 'production',
    target: 'node',
    devtool: env.development ? 'cheap-eval-source-map' : false,  
    resolve: {
        extensions: ['.ts', '.js'],
        modules: ['node_modules', 'src', 'package.json'],
    },
    module: {
        rules: [
            {
                test: /\.ts$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
        ],
    },
    externals: [nodeExternals()]
});

const frontConfig = (env = {}) => ({
    entry: ['./src/frontend/main.js'],
    output: {
        filename: 'SleepLockerFrontend.js',
        path: __dirname + '/dist/frontend'
    },
    mode: env.development ? 'development' : 'production',
    resolve: {
        extensions: ['.ts', '.js'],
        modules: ['node_modules', 'src', 'package.json'],
    },
    module: {
        rules: [
            {
                test: /\.ts$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
            {
                test: /\.css$/i,
                use: ['style-loader', 'css-loader'],
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: {
                    loader: 'elm-webpack-loader',
                    options: {
                        optimize: env.development ? false : true
                    }
                }
            }
        ]
    },
    externals: {
    }
});

module.exports = [frontConfig, backConfig]