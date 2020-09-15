declare module 'sourcemapped-stacktrace' {

    export interface MapStackTraceOptions {
        /** Filter function applied to each stackTrace line. Lines which do not pass the filter won't be processesd. */
        filter?: (line: string) => boolean
        /** Whether to cache sourcemaps globally across multiple calls. */
        cacheGlobally?: boolean
        /** Whether to use synchronous ajax to load the sourcemaps. */
        sync?: boolean
    }
    
    /**
     * Re-map entries in a stacktrace using sourcemaps if available.
     *
     * @param stack The stacktrace from the browser (`error.stack`).
     * @param done Callback invoked with the transformed stacktrace.
     * @param opts Options object.
     */
    export function mapStackTrace(stack: string | undefined, done: (mappedStack: string[]) => void, opts?: MapStackTraceOptions): void

}
