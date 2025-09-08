package com.sgm.navi.scene.impl.navi.inter;


import android.app.Activity;
import android.graphics.drawable.BitmapDrawable;

import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.define.navi.HandCardType;
import com.sgm.navi.service.define.navi.NaviParkingEntity;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.position.LocParallelInfoEntity;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;

import java.util.HashMap;
import java.util.List;

public interface ISceneCallback {

    /**
     * 跳转到沿途搜页面
     */
    default void skipAlongWayFragment() {
    }

    /**
     * 跳转到设置页面
     */
    default void skipSettingFragment() {
    }

    /**
     * 跳转到引导路线偏好scene
     */
    default void skipNaviPreferenceScene() {
    }

    /**
     * 跳转到引导控制条更多scene
     */
    default void skipNaviControlMoreScene() {
    }

    /**
     * 跳转到引导控制条scene
     */
    default void skipNaviControlScene() {
    }

    /**
     * 显示/隐藏场景页面
     *
     * @param sceneType 场景类型
     * @param isVisible 是否可见
     */
    default void updateSceneVisible(NaviSceneId sceneType, boolean isVisible) {
    }

    /**
     * @param entity 删除途经点
     */
    default void deleteViaPoint(NaviViaEntity entity) {

    }

    /**
     * 更新导航途经点已到达
     */
    default void onUpdateViaPass() {
    }

    /**
     * 跳转到导航服务区/收费站详情页面
     *
     * @param type           service or toll
     * @param sapaInfoEntity 服务区/收费站信息
     */
    default void skipNaviSapaDetailScene(int type, SapaInfoEntity sapaInfoEntity) {

    }

    /***
     * 搜索新的充电站
     */
    default void searchNewChargeStation() {

    }

    /***
     * 打开补能规划
     */
    default void openSupplyPlan() {

    }

    /**
     * 添加途径点
     *
     * @param info 替换充电站信息
     */
    default void addViaList(final RouteAlterChargeStationInfo info) {

    }

    /***
     * 去充电，重新发起新的导航
     */
    default void goCharge() {

    }

    /**
     * 显示控制卡片详情页面
     */
    default void showControlDetails() {

    }

    /**
     * 跳转到搜索页面
     *
     * @param keyWord    搜索关键字
     * @param searchType 搜索类型
     */
    default void goSearchView(final String keyWord, final int searchType) {
    }

    /**
     * 跳转到沿途搜页面
     */
    default void goAlongWayList() {
    }

    /**
     * 关闭搜索页面
     */
    default void closeSearchView() {

    }

    /***
     * 推荐的目的地，选择立即导航
     */
    default long startNaviRightNow(final PoiInfoEntity poiInfo) {
        return -1;
    }

    /***
     * 展示推荐列表，包括：充电站列表
     * @param searchResultEntity
     */
    default void showRecChargeList(final SearchResultEntity searchResultEntity) {

    }


    /***
     * 展示推荐列表，包括：充电站列表
     * @param searchResultEntity
     */
    default void showRecGasList(final SearchResultEntity searchResultEntity) {

    }

    /***
     * 展示推荐列表，包括：充电站列表
     * @param list
     */
    default void showRecParkList(final List<NaviParkingEntity> list) {

    }

    default void showHandingCardDetail(final List<PoiInfoEntity> list, final HandCardType type) {

    }
    /**
     * 返回到导航页面
     */
    default void backToNaviFragment() {

    }

    /**
     * @return 是否是导航页面
     */
    default boolean getCurrentFragmentIsNavi() {
        return false;
    }

    /**
     * 获取到是否有页面需要保持全览状态，当前有途经点列表，停车场列表，充电站列表，加油站列表
     * @return 是否需要预览列表
     */
    default boolean isNeedPreViewShowList() {
        return false;
    }

    /**
     * 隐藏导航页面，只保留继续导航按钮
     */
    default void hideNaviContent() {

    }

    /**
     * 停止语音播放
     */
    default void stopSpeech() {

    }

    /**
     * 更新接近动作图标
     * @param resource resource
     * @param drawable drawable
     */
    default void updateNextIcon(int resource, BitmapDrawable drawable) {

    }

    /**
     * 更新接近动作状态
     * @param isVisible 是否可显示
     * @param isOffLine 是否是离线
     */
    default void updateNextStatus(boolean isVisible, boolean isOffLine) {

    }

    /**
     * @return 返回下一个动作实体
     */
    default NextManeuverEntity getNextManeuverEntity() {
        return null;
    }

    default void updateNextText(String text) {

    }


    default void clickGoHomeBtn(int type){}

    default HashMap<NaviSceneId, Integer> getSceneStatus() {
        return null;
    }

    default void closeNavi() {}

    default void cancelClusterOverViewTimer() {}

    default Activity getActivity() {
        return null;
    }

    default boolean getIsViaArrived() {
        return false;
    }

    default void saveHandingCardDetail(List<PoiInfoEntity> infoEntities, HandCardType type) {}

    default void setChargeTipEntity(Object chargeTipEntity) {}

    default void setEndPoint(PoiInfoEntity endPoint) {}
    default void setLocParallelInfoEntity(LocParallelInfoEntity entity) {}
}
