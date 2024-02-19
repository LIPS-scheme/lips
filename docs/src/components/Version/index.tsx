import { useEffect, useState } from 'react';
import BrowserOnly from '@docusaurus/BrowserOnly';

function Version(): JSX.Element {
    const [version, setVersion] = useState<string>('');
    useEffect(() => {
        (function delay() {
            if ((window as any).lips == undefined) {
                setTimeout(delay, 50);
            } else {
                setVersion((window as any).lips.version);
            }
        })();
    }, []);
    return <span>{version}</span>;
}

export default function BrowserVersion(): JSX.Element {
  return (
    <BrowserOnly fallback={<span>...</span>}>
      {() => {
        return <Version/>
      }}
    </BrowserOnly>
  );
}
