import Heading from '@theme/Heading';
import Link from '@docusaurus/Link';
import styles from './styles.module.css';

const ScreenshotSVG = require('@site/static/img/screenshot.svg').default;

export default function Screenshooter(): JSX.Element {
  return (
    <section id="bookmarklet">
      <div className="container">
        <ScreenshotSVG className={styles.svg} role="img" />
        <Heading as="h3">
          <Link to="/screenshooter">Screenshooter</Link>
        </Heading>
        <p>This tool allow you to create nice screenshot of code. The screenshot tool is aware of
        any syntax added on top of Scheme. The tool is created using Codemirror and custom style
        build on top of Scheme syntax.</p>
        <p>You can use this tool to generate snippets of Scheme or LIPS Code and share it on social media.</p>
      </div>
    </section>
  );
}
