import Heading from '@theme/Heading';
import Head from '@docusaurus/Head';
import styles from './styles.module.css';

const BookmarkSVG = require('@site/static/img/bookmarklet.svg').default;

export default function Bookmark(): JSX.Element {
    return (
        <section id="bookmarklet">
          <div className="container">
            <BookmarkSVG className={styles.bookmarkSVG} role="img" />
            <Heading as="h3">Bookmarklet</Heading>
            <Head>
              <script src="/js/bookmark.js"/>
            </Head>
            <p>....</p>
            <p>
              When you're learning Scheme language, you can run the REPL directly on any page that
              have Scheme tutorial you're learning from. It even work with PDF files and new empty tab
              (at least in Chrome). Drag this link <a id="bookmark_link">LIPS REPL</a> to your
              bookmarks. When you click on the bookmark it will run the interpreter. You can also just
              click the link.</p> <p>The bookmark can also be used to add REPL to your LIPS Web
              application.</p> <p>It may also not work no sites that are protected with{' '}
              <a href="https://en.wikipedia.org/wiki/Content_Security_Policy">
                Content Security Policy
              </a>
            </p>
          </div>
        </section>
    );
};
