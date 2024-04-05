import Heading from '@theme/Heading';

import styles from './styles.module.css';

import Bookmark from './Bookmark';
import Screenshooter from './Screenshooter';

export default function Tools(): JSX.Element {
  return (
    <div className={styles.tools}>
      <Heading as="h2" id="tools" className={styles.header}>Tools</Heading>
      <Bookmark />
      <Screenshooter />
    </div>
  );
}
