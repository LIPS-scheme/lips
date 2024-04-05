import Heading from '@theme/Heading';
import styles from '../styles.module.css';

export default function License(): JSX.Element {
    return (
      <div className={styles.section}>
        <Heading as="h2" id="license" className={styles.header}>License</Heading>
        <p>
          LIPS Scheme is <a href="https://opensource.com/resources/what-open-source">Open Source</a>
          {' '}and released on <a href="https://github.com/jcubic/lips/blob/master/LICENSE">MIT license</a></p>
      </div>
    );
}
