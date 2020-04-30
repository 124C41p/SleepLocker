'use strict';

const NodemonPlugin = require('nodemon-webpack-plugin');
const nodeExternals = require('webpack-node-externals');

module.exports = (env = {}) => {
    const config = {
        entry: ['./src/main.ts'],
        output: {
            filename: 'SleepLocker.js',
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
        externals: [nodeExternals()],
        plugins: []
    };

    if (env.nodemon) {
        config.watch = true;
        config.plugins.push(new NodemonPlugin());
    }

    return config;
};