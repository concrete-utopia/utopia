export function projectContentDirectory(fullPath, dir, children) {
    return {
        type: 'PROJECT_CONTENT_DIRECTORY',
        fullPath: fullPath,
        directory: dir,
        children: children,
    };
}
export function projectContentFile(fullPath, content) {
    return {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: fullPath,
        content: content,
    };
}
export function directory() {
    return {
        type: 'DIRECTORY',
    };
}
export function imageFile(imageType, base64, width, height, hash, gitBlobSha) {
    return {
        type: 'IMAGE_FILE',
        imageType: imageType,
        base64: base64,
        width: width,
        height: height,
        hash: hash,
        gitBlobSha: gitBlobSha,
    };
}
export function assetFile(base64, gitBlobSha) {
    return {
        type: 'ASSET_FILE',
        base64: base64,
        gitBlobSha: gitBlobSha,
    };
}
export function textFile(fileContents, lastSavedContents, lastParseSuccess, versionNumber) {
    return {
        type: 'TEXT_FILE',
        fileContents: fileContents,
        lastSavedContents: lastSavedContents,
        lastParseSuccess: lastParseSuccess,
        versionNumber: versionNumber,
    };
}
export function textFileContents(code, parsed, revisionsState) {
    return {
        code: code,
        parsed: parsed,
        revisionsState: revisionsState,
    };
}
