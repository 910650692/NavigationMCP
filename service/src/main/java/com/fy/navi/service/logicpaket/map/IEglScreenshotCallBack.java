package com.fy.navi.service.logicpaket.map;

import com.fy.navi.service.define.map.MapType;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/5/16
 * Description: [在这里描述文件功能]
 */
public interface IEglScreenshotCallBack {
    default void onEGLScreenshot(MapType mapType, byte[] bytes) {}
}
