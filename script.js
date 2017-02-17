const fs = require('fs');

const dirs = fs.readdirSync('050').map(x => '050/' + x).concat(fs.readdirSync('100').map(x => '100/' + x));

dirs.map(pd);

function pd(dir) {
    const app = fs.readFileSync(dir + '/app/Main.hs').toString();
    const srcs = fs.readdirSync(dir + '/src')
          .filter(x => x.endsWith('.hs'))
          .map(x => dir + '/src/' + x)
          .map(x => fs.readFileSync(x).toString());
    const merged = merge(srcs.concat(app));

    let moduleName = dir.split('/')[dir.split('/').length - 1];
    moduleName = moduleName[0].toUpperCase() + moduleName.slice(1);

    const s = toS(moduleName, merged);
    fs.writeFileSync('src/' + moduleName + '.hs', s);
}

function merge(files) {
    return files.reduce((result, file) => {
        const p = parse(file);
        return {
            langs: m(result.langs, p.langs),
            imports: m(result.imports, p.imports.filter(x => !x.endsWith('Euler'))),
            fns: result.fns.concat(p.fns),
        };
    }, {
        langs: [],
        imports: [],
        fns: []
    });
}

function m(xs, ys) {
    ys.forEach(y => {
        if (!xs.includes(y)) {
            xs.push(y);
        }
    });
    return xs.sort();
}

function parse(file) {
    const lines = file.split('\n');
    const result = {
        langs: [],
        imports: [],
        fns: []
    };

    let im = false;
    let done = false;
    while (true) {
        let l = lines.shift();
        if (typeof l === 'undefined') {
            break;
        } else if (l.startsWith('{-#')) {
            result.langs.push(l);
        } else if (l.startsWith('import ')) {
            im = true;
            result.imports.push(l);
        } else if (l.startsWith('module')) {
            while (!l.includes('where')) {
                l = lines.shift();
            }
            continue;
        } else if (l.startsWith('someFunc')) {
            continue;
        } else {
            result.fns.push(l);
        }
    }

    for (let i = 0; i < result.fns.length;) {
        if (i === 0 && result.fns[i] === '') {
            result.fns.splice(i, 1);
        } else if (result.fns[i] === '' && result.fns[i + 1] === '') {
            result.fns.splice(i, 1);
        } else {
            i++;
        }
    }

    return result;
}

function toS(moduleName, parsed) {
    const langs = parsed.langs.length > 0 ? parsed.langs.join('\n') + '\n\n' : '';
    const imports = parsed.imports.length > 0 ? parsed.imports.join('\n') + '\n\n' : '';
    return langs + 'module ' + moduleName + ' where\n\n' + imports + parsed.fns.join('\n');
}
