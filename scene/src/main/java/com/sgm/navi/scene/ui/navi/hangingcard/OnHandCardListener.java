package com.sgm.navi.scene.ui.navi.hangingcard;

import com.sgm.navi.service.define.navi.HandCardType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/16
 * Description: [在这里描述文件功能]
 */
public interface OnHandCardListener {
    /***
     * 重置计时器并暂停倒计时
     * @param type
     */
    void onResetAndStopTimer(HandCardType type);

    /***
     * 重启计时器
     * @param type
     */
    void restartTimer(HandCardType type);

    /***
     * 删除悬挂卡
     * @param type
     */
    void deleteHandCard(HandCardType type);

    /***
     * 立即导航，改变目的地
     * @param poiInfo
     */
    void naviNow(PoiInfoEntity poiInfo, HandCardType type);

    /***
     * 把折叠的卡片摊开显示，并隐藏控制栏
     */
    void expandHandingCardAndHideControlBar();

    /***
     * 展示全部列表
     */
    void showAll(List<PoiInfoEntity> allData, HandCardType type);
}
