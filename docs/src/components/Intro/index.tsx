import Heading from '@theme/Heading';

import styles from '../styles.module.css';

export default function Intro(): JSX.Element {
  return (
    <div className={styles.section}>
      <Heading as="h2" id="summary" className={styles.header}>Summary</Heading>
      <p>
        LIPS Scheme is powerful lisp interpreter written in JavaScript. The aim of the project
        is to fully support <a href="https://standards.scheme.org/#r7rs">R7RS</a> specification
        of Scheme Programming Language. But add more powerful features on top.
      </p>
      <p>
        The name LIPS is recursive acronym
        which stands for <strong>LIPS Is Pretty Simple</strong>. Over time the project may no
        longer be that simple in implementation but it always be LIPS.
      </p>
      <Heading as="h2" id="features" className={styles.header}>Features</Heading>
      <p>Most important features of LIPS:</p>
      <ul>
        <li>Great integration with JavaScript. You can use any JS library out of the box.</li>
        <li>Asynchronous execution (auto resolving of promises with optional promise quotation).</li>
        <li>Literal regular expression.</li>
        <li>Functional helpers (inspired by <a href="https://ramdajs.com/">RamdaJS</a> and <a href="https://lodash.com/">Lodash</a>).</li>
        <li>Possibility modify the parser at runtime (add new syntax, similar to vectors and object).</li>
        <li>Possibility to set representation of new data types for write and display. With parser extensions you can make new data types have different syntax and still be homoicoic.</li>
        <li>Small JavaScript core with Standard Library implemented in basic Scheme.</li>
        <li>Full support of Unicode and Emoji.</li>
        <li>Support for <a href="https://en.wikipedia.org/wiki/SXML">SXML</a>, that allow to write <a href="https://en.wikipedia.org/wiki/JSX_(JavaScript)">JSX</a> e.g. with <a href="https://preactjs.com/">Preact</a> or <a href="https://react.dev/">React</a> apps.</li>
        <li>I/O Ports supports in browser with <a href="https://github.com/jvilk/BrowserFS">BrowserFS</a>.</li>
        <li>Full numerical tower and Big Integer support (still <a href="https://github.com/jcubic/lips/issues/34">not 100% fully unit tested</a>).</li>
        <li>Powerful introspection (similar to the one in Python).</li>
        <li>Auto formatting of lisp code (pretty print).</li>
        <li>Lisp/hygienic macros and macroexpand.</li>
        <li>Builtin help system.</li>
      </ul>
    </div>
  );
}
