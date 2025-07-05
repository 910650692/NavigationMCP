package com.sgm.navi.hmi.launcher;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/7/4
 * Description: [桌面Widgets显示和隐藏回调监听]
 */
public interface OnDeskCardVisibleStateChangeListener {
    // isVisible: true: 显示，false: 隐藏
    void onDeskCardVisibleStateChange(boolean isVisible);
}
