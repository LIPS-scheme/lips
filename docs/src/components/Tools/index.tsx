import styles from './styles.module.css';

import Bookmark from './Bookmark';
import Screenshooter from './Screenshooter';

export default function Tools(): JSX.Element {
  return (
    <div className={styles.tools}>
      <Bookmark />
      <Screenshooter />
    </div>
  );
}
