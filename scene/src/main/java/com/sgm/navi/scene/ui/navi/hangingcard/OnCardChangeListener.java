package com.sgm.navi.scene.ui.navi.hangingcard;

import com.sgm.navi.service.define.navi.HandCardType;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/18
 * Description: [悬挂卡事件回调]
 */
public interface OnCardChangeListener {
    /***
     * 倒计时结束
     * @param type
     */
    void onTimerFinished(HandCardType type);

    /***
     * 改变路线成功
     */
    void onChangeDestinationSuccess(HandCardType type);

    /***
     * 展示列表详情
     */
    void onShowDetail(HandCardType type);

    /***
     * 展开列表
     */
    void expandAll();
}
