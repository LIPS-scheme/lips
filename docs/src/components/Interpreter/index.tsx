import { useLayoutEffect, useRef, useState, MouseEvent, CSSProperties } from 'react';
import Markdown from 'react-markdown';
import clsx from 'clsx';

import useIsBrowser from '@docusaurus/useIsBrowser';
import Head from '@docusaurus/Head';
import useScripts from '@site/src/hooks/useScripts';
import CodeBlock from '@theme/CodeBlock';
import { initTerminal, destroyTerminal } from './terminal';
import examples from './examples';

import './styles.css';

export interface TerminalProps extends CSSProperties {
  '--size': string;
}

function track(type: string, command: string) {
  const umami = globalThis.umami as any;
  if (umami) {
    umami.track('REPL', { type, command });
  }
}

export type JQueryTerminal = ReturnType<typeof globalThis.terminal>;

const replReady = () => {
    return (
        globalThis.jQuery &&
        globalThis.jQuery.terminal &&
        globalThis.terminal &&
        globalThis.lips
    );
}

// monkey patch keymap added by LIPS terminal
function trackCommands(term: JQueryTerminal) {
  const ENTER = term.cmd().keymap('ENTER');
  term.cmd().keymap('ENTER', function(e: KeyboardEvent, orig: () => any) {
    const command = term.get_command();
    track('command', command);
    return ENTER(e, orig);
  });
}

export default function Interpreter(): JSX.Element {
  const [activeSnippet, setActiveSnippet] = useState(0);
  const [size, setSize] = useState(1);
  const ref = useRef<HTMLDivElement>();

  const isProd = process.env.NODE_ENV === 'production';
  const isBrowser = useIsBrowser();
  const isStatic = isProd && !isBrowser && !globalThis.jQuery;

  useScripts(!globalThis.jQuery && [
    'https://cdn.jsdelivr.net/npm/jquery',
    'https://cdn.jsdelivr.net/combine/gh/jcubic/jquery.terminal@8a179e7b2c9/js/jquery.terminal.min.js,npm/js-polyfills/keyboard.js,npm/prismjs/prism.js,npm/jquery.terminal@latest/js/prism.js,npm/prismjs/components/prism-scheme.min.js',
    'https://cdn.jsdelivr.net/gh/jcubic/lips@d730e075d8/lib/js/terminal.js',
    'https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/prism.js'
  ]);

  useLayoutEffect(() => {
    (function loop() {
      if (replReady() && styleReady()) {
        const term = initTerminal();
        trackCommands(term);
      } else {
        setTimeout(loop, 100);
      }
    })();
    return destroyTerminal;
  }, []);

  function execSnippet(selector = '.example:visible') {
    const $ = globalThis.jQuery;
    const $snippet = $(selector);
    const code = $snippet.text();
    const index = $snippet.closest('li').index();
    const term = $('.term').terminal();
    term.echo(term.get_prompt(), { formatters: false });
    term.exec(code, true);
    track('snippet', index + 1);
    if (typeof screen.orientation === 'undefined') {
      setTimeout(() => term.focus(), 0);
    }
  }

  function makeChangeSnippet(index: number) {
    return (event: MouseEvent<HTMLAnchorElement>) => {
      event.preventDefault();
      setActiveSnippet(index);
    };
  }

  function sizeInrement(increment: number) {
    return (event: MouseEvent<HTMLAnchorElement>) => {
      event.preventDefault();
      setSize(size => size + increment);
    };
  }

  function fullScreen(event: MouseEvent<HTMLAnchorElement>) {
    event.preventDefault();
    const $ = globalThis.jQuery;
    $(document.body).addClass('full-screen');
  }

  function exitFullScreen(event: MouseEvent<HTMLAnchorElement>) {
    event.preventDefault();
    const $ = globalThis.jQuery;
    $(document.body).removeClass('full-screen');
  }

  function styleReady() {
    // hack to prevent initalizaing of jQuery Terminal before style is loaded
    return !!getComputedStyle(ref.current).getPropertyValue('--base-background');
  }

  const terminalStyle = {
    '--size': size.toFixed(1)
  } as TerminalProps;

  return (
    <>
      <Head>
        <link rel="preconnect" href="https://cdn.jsdelivr.net" />
        <link href="https://cdn.jsdelivr.net/combine/npm/jquery.terminal@latest/css/jquery.terminal.min.css,npm/terminal-prism@0.4.1/css/prism-coy.css" rel="stylesheet"/>
        <link href="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/css/terminal.css"
              rel="stylesheet"/>
        {isStatic && <script src="https://cdn.jsdelivr.net/npm/jquery" />}
        {isStatic && <script src="https://cdn.jsdelivr.net/combine/gh/jcubic/jquery.terminal@devel/js/jquery.terminal.min.js,npm/js-polyfills/keyboard.js,npm/prismjs/prism.js,npm/jquery.terminal@latest/js/prism.js,npm/prismjs/components/prism-scheme.min.js" />}
        {isStatic && <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/terminal.js" />}
        {isStatic && <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/prism.js" />}
        {!globalThis.lips && <script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"
                                     data-bootstrap="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/std.xcb"/>}
      </Head>
      <div className="intro">
        <div className="actions-wrapper">
          <ul className="actions">
            <li className="zoom-in icon">
              <a href="#" onClick={sizeInrement(0.1)} title="Zoom In">Zoom In</a>
            </li>
            <li className="zoom-out icon">
              <a href="#" onClick={sizeInrement(-0.1)} title="Zoom Out">Zoom Out</a>
            </li>
            <li className="full-screen">
              <ul>
                <li className="full-screen icon">
                  <a href="#" onClick={fullScreen} title="Full Screen">Full Screen</a>
                </li>
                <li className="exit-full-screen icon">
                  <a href="#" onClick={exitFullScreen} title="Exit Full Screen">Exit Full Screen</a>
                </li>
              </ul>
            </li>
          </ul>
        </div>
        <div className="terminal marker" ref={ref}></div>
        <div className="term" style={terminalStyle}>
          <div className="loader-container">
            <div className="loader">
              <div>.</div>
              <div>..</div>
              <div>...</div>
              <div>....</div>
              <div>.....</div>
              <div>......</div>
            </div>
          </div>
        </div>
        <div className="examples terminal-external">
          <div className="egg">
            <button onClick={() => execSnippet('li.hidden .example')}>π</button>
          </div>
          <button className="run" onClick={() => execSnippet()}>run</button>
          <ul className="list">
            {examples.map((example, index) => {
              return (
                <li key={index} className={clsx({active: index === activeSnippet, hidden: example.hidden})}>
                  <div className="example">
                    <CodeBlock language="scheme" className="lips">
                      {example.code}
                    </CodeBlock>
                  </div>
                  <div className="description"><Markdown>{example.description}</Markdown></div>
                </li>
              );
            })}
          </ul>
          <ul className="pagination">
            {examples.map((example, index) => {
              return (
                <li key={index} className={clsx({active: index === activeSnippet, hidden: example.hidden})}>
                  <a href="#" onClick={makeChangeSnippet(index)}>{ index + 1 }</a>
                </li>
              );
            })}
          </ul>
        </div>
      </div>
    </>
  );
};
