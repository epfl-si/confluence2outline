import sys
import threading
from queue import Queue
from datetime import datetime
from stat import S_IFREG

from functools import cached_property

from stream_unzip import stream_unzip
from stream_zip import ZIP_32, stream_zip


class ZipSource:
    def __init__ (self, path):
        self.path = path

    def files (self):
        with open(self.path, "rb") as f:
            for b_filename, file_size, unzipped_chunks in stream_unzip(f):
                zf = ZipFile(b_filename, file_size, unzipped_chunks)
                yield zf
                zf.close()

    def read_file (self, path):
        pass


class ZipSink:
    def __init__ (self, output_path):
        self._output_path = output_path
        self._queue = Queue(maxsize=1)
        self._last = object()
        self._zipper_thread = threading.Thread(target=self._zipper_thread_body)
        self._zipper_thread.start()
        self._timestamp = datetime.now()

    def _zipper_thread_body (self):
        with open(self._output_path, "wb") as f:
            for chunk in stream_zip(self._yield_from_queue()):
                f.write(chunk)

    def _yield_from_queue (self):
        """Runs in `self._zipper_thread`. Yields whatever comes out of `self._queue`."""
        while True:
            next = self._queue.get()
            if next is self._last:
                self._queue.task_done()
                break
            else:
                yield next
                self._queue.task_done()

    def add (self, zip_file_or_text, as_filename=None):
        if as_filename is not None:
            t_filename = as_filename
        else:
            t_filename = zip_file_or_text.b_filename.decode("utf-8")

        if isinstance(zip_file_or_text, str):
            chunks = [zip_file_or_text.encode('utf-8')]
        else:
            chunks = zip_file_or_text.chunks

        self._queue.put((t_filename, self._timestamp, S_IFREG | 0o600, ZIP_32,
                         chunks))
        self._queue.join()

    def close (self):
        self._queue.put(self._last)
        self._zipper_thread.join()


class ZipFile:
    def __init__ (self, b_filename, file_size, unzipped_chunks):
        self.b_filename = b_filename
        self.chunks = unzipped_chunks

    def close (self):
        # Prevent stream_unzip.UnfinishedIterationError:
        for _discard in self.chunks:
            pass

    @property
    def path (self):
        return self.b_filename.decode("utf-8")
