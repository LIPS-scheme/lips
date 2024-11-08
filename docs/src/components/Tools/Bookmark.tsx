import { useEffect, useRef } from 'react';
import Heading from '@theme/Heading';
import useBrokenLinks from '@docusaurus/useBrokenLinks';

import styles from './styles.module.css';
const BookmarkSVG = require('@site/static/img/bookmarklet.svg').default;

export default function Bookmark(): JSX.Element {
  const ref = useRef<HTMLAnchorElement>();
  useBrokenLinks().collectAnchor('bookmark');
  useEffect(() => {
    const url = 'https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/js/bookmark.js';
    fetch(url).then(function(res) {
        return res.text();
    }).then(text => {
      if (ref.current) {
        ref.current.href = text;
      }
    });
  }, [ref.current]);

  return (
    <section id="bookmark">
      <div className="container">
        <BookmarkSVG className={styles.svg} role="img" />
        <Heading as="h3">Bookmarklet</Heading>
        <p>
          When you're learning Scheme language, you can run the REPL directly on any page that
          have Scheme tutorial you're learning from. It even work with PDF files and new empty tab
          (at least in Chrome). Drag this link <a id="bookmark_link" ref={ref}>LIPS REPL</a> to your
          bookmarks. When you click on the bookmark it will run the interpreter. You can also just
          click the link.</p> <p>The bookmark can also be used to add REPL to your LIPS Web
          application.</p> <p>It may not work on sites that are protected with{' '}
          <a href="https://en.wikipedia.org/wiki/Content_Security_Policy">Content Security Policy</a>.
          CSP was created mostly as a security solution to prevent
          {' '}<abbr title="Cross-site scripting">XSS</abbr> vunerablities. You can disable this
          mechanism with <a href="http://tinyurl.com/CSP-disable">Chrome Extension</a>,
          but you do this on your own risk.
        </p>
      </div>
    </section>
  );
}
