import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

fs.readFile(path.join(__dirname, '../package.json'), 'utf8').then(json => {
  const { version } = JSON.parse(json);
  return fs.writeFile(path.join(__dirname, 'version.json'), JSON.stringify(version));
});
