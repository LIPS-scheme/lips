import Heading from '@theme/Heading';
import Head from '@docusaurus/Head';
import Link from '@docusaurus/Link';
import styles from './styles.module.css';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

const BookmarkSVG = require('@site/static/img/bookmarklet.svg').default;
const ScreenshotSVG = require('@site/static/img/screenshot.svg').default;

function Bookmark(): JSX.Element {
    const { siteConfig } = useDocusaurusContext();

    return (
        <section id="bookmarklet">
          <div className="container">
            <BookmarkSVG className={styles.svg} role="img" />
            <Heading as="h3">Bookmarklet</Heading>
            <Head>
              <script src={`${siteConfig.baseUrl}/js/bookmark.js`}/>
            </Head>
            <p>
              When you're learning Scheme language, you can run the REPL directly on any page that
              have Scheme tutorial you're learning from. It even work with PDF files and new empty tab
              (at least in Chrome). Drag this link <a id="bookmark_link">LIPS REPL</a> to your
              bookmarks. When you click on the bookmark it will run the interpreter. You can also just
              click the link.</p> <p>The bookmark can also be used to add REPL to your LIPS Web
              application.</p> <p>It may also not work on sites that are protected with{' '}
              <a href="https://en.wikipedia.org/wiki/Content_Security_Policy">
                Content Security Policy
              </a>
            </p>
          </div>
        </section>
    );
};

function Screeshooter(): JSX.Element {
  return (
    <section id="bookmarklet">
      <div className="container">
        <ScreenshotSVG className={styles.svg} role="img" />
        <Heading as="h3"><Link to="/screenshooter">Screenshooter</Link></Heading>
        <p>This tool allow you to create nice screenshot of code. The screenshot tool is aware of
        any syntax added on top of Scheme. The tool is created using Codemirror and custom style
        build on top of Scheme syntax.</p>
        <p>You can use this tool to generate snippets of Scheme or LIPS Code and share it on social media.</p>
      </div>
    </section>
  );
}

export default function Tools(): JSX.Element {
  return (
    <div className={styles.tools}>
      <Bookmark />
      <Screeshooter />
    </div>
  );
}
