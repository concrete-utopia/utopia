/**
 * TODO: change this to the correct types
 */
type ParseSuccess = null;
type ParsedTextFile = {
    type: 'UNPARSED';
};
export type ProjectContentTreeRoot = {
    [key: string]: ProjectContentsTree;
};
export type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile;
export interface ProjectContentDirectory {
    type: 'PROJECT_CONTENT_DIRECTORY';
    fullPath: string;
    directory: Directory;
    children: ProjectContentTreeRoot;
}
export declare function projectContentDirectory(fullPath: string, dir: Directory, children: ProjectContentTreeRoot): ProjectContentDirectory;
export interface ProjectContentFile {
    type: 'PROJECT_CONTENT_FILE';
    fullPath: string;
    content: Content;
}
export declare function projectContentFile(fullPath: string, content: Content): ProjectContentFile;
export interface Directory {
    type: 'DIRECTORY';
}
export declare function directory(): Directory;
export type Content = ImageFile | AssetFile | TextFile;
export interface ImageFile {
    type: 'IMAGE_FILE';
    imageType?: string;
    base64?: string;
    width?: number;
    height?: number;
    hash: number;
    gitBlobSha?: string;
}
export declare function imageFile(imageType: string | undefined, base64: string | undefined, width: number | undefined, height: number | undefined, hash: number, gitBlobSha: string | undefined): ImageFile;
export interface AssetFile {
    type: 'ASSET_FILE';
    base64?: string;
    gitBlobSha?: string;
}
export declare function assetFile(base64: string | undefined, gitBlobSha: string | undefined): AssetFile;
export interface TextFile {
    type: 'TEXT_FILE';
    fileContents: TextFileContents;
    lastSavedContents: TextFileContents | null;
    lastParseSuccess: ParseSuccess | null;
    versionNumber: number;
}
export declare function textFile(fileContents: TextFileContents, lastSavedContents: TextFileContents | null, lastParseSuccess: ParseSuccess | null, versionNumber: number): TextFile;
export interface TextFileContents {
    code: string;
    parsed: ParsedTextFile;
    revisionsState: RevisionsStateType;
}
export declare function textFileContents(code: string, parsed: ParsedTextFile, revisionsState: RevisionsStateType): TextFileContents;
export type RevisionsStateType = 'PARSED_AHEAD' | 'CODE_AHEAD' | 'BOTH_MATCH' | 'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT';
export {};
