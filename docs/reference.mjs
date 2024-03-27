import fs from 'fs';

import { env } from '../dist/lips.esm.js';

function skip_internal([name]) {
    return name.match(/^%/) === null;
}

function map_docs(pairs) {
    return pairs.filter(skip_internal).map(([name, fn]) => {
        return {
            name,
            doc: fn?.__doc__
        };
    }).filter(object => object.doc);
}
function get_docs_strings() {
    const global_env = env.__parent__.__env__;
    const docs = map_docs(Object.entries(global_env));
    docs.sort((a, b) => a.name.localeCompare(b.name));
    return docs;
}

fs.writeFile('reference.json', JSON.stringify(get_docs_strings()), (err) => {
    if (err) {
        console.log(err);
    }
});
