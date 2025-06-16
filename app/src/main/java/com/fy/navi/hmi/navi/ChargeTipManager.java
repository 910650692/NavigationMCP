package com.fy.navi.hmi.navi;


import android.annotation.SuppressLint;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.ui.navi.ChargeTipEntity;
import com.fy.navi.scene.ui.navi.SceneNaviChargeBtnType;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.FyChargingStation;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.speech.SpeechPackage;

import java.util.List;

public class ChargeTipManager {
    private static final String TAG = "ChargeTipManager";
    private static final double EDGE_DIS = 50;// 单位：KM
    private static final String MSG_ACTION_KNOW = "知道了";
    private static final String MSG_ACTION_SEARCH_NEW = "查找新站";
    private static final String MSG_ACTION_GO_CHARGING = "去充电";

    private static final String TIP_MSG_1 = "续航里程不足50km";
    private static final String TIP_MSG_2 = "%s电站目前拥挤";
    private static final String TIP_MSG_2_TTS = "%s充电站目前拥挤，是否为您查找沿途充电站？";
    private static final String TIP_MSG_3 = "已为您刷新了补能规划";
    private static final String TIP_MSG_3_SUB_TITLE = "%s充电站目前未营业";
    private static final String TIP_MSG_3_TTS = "%s充电站目前未营业，已为您刷新了补能规划";
    private static final String TIP_MSG_4 = "已为您刷新了补能规划";
    private static final String TIP_MSG_4_SUB_TITLE = "%s充电站即将停止营业";
    private static final String TIP_MSG_4_TTS = "%s充电站即将停止营业，已为您刷新了补能规划";
    private static final String TIP_MSG_5 = "电量无法抵达%s充电站";
    private static final String TIP_MSG_5_TTS = "电量无法抵达%s充电站，是否为您查找沿途充电站？";
    private static final String TIP_MSG_6 = "剩余里程内充电站较少";
    private static final String TIP_MSG_7 = "预约充电站%s分钟后超时";
    private static final String TIP_MSG_8 = "预约充电桩地锁已打开";
    private static final String TIP_MSG_9 = "%s充电站即将停止营业";
    private static final String TIP_MSG_10 = "%s充电站目前未营业";
    private static final String TIP_MSG_11 = "您错过了%s充电站";

    private NaviGuidanceViewModel mViewModel;
    private PositionPackage mPositionPackage;
    private LayerPackage mLayerPackage;
    private SpeechPackage mSpeechPackage;
    private NaviEtaInfo.NaviTimeAndDist mViaChargeStation;// 即将经过的途径充电站
    private boolean mViaChargeStationHasFree = true; // TODO 即将通过的充电站是否已饱和，数据应该根据接口查询
    private boolean mTestIsMocking = false;
    private FyElecVehicleETAInfo mEtaInfo; // 能量耗尽点，如果为null就是不存在

    public ChargeTipManager(final NaviGuidanceViewModel naviGuidanceModel) {
        this.mViewModel = naviGuidanceModel;
        mPositionPackage = PositionPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSpeechPackage = SpeechPackage.getInstance();
    }

    /***
     * 设置下一个即将通过的充电站
     * @param naviInfoBean
     */
    public void setNextViaChargeStation(final NaviEtaInfo naviInfoBean) {
        if (naviInfoBean == null || ConvertUtils.isEmpty(naviInfoBean.ChargeStationRemain)) {
            Logger.i(TAG, "当前路线上不存在充电站！");
            this.mViaChargeStation = null;
            return;
        }
        this.mViaChargeStation = naviInfoBean.ChargeStationRemain.get(0);
        Logger.i(TAG, "当前路线上存在充电站途径点且已保存成功！：" , mViaChargeStation.time);
    }

    /***
     * 释放资源
     */
    public void unInit() {
        mViewModel = null;
        mTestIsMocking = false;
        Logger.i(TAG, "unInit success!");
    }

    /***
     * 当充电桩经过的时候回调
     * 错过了某一推荐站点，但不能为用户合理规划新补能路线，LBS需要提示用户:
     */
    public void onUpdateChargeStationPass() {
        if (!hasEnergyEndPoint()) {
            return;
        }
        if (ConvertUtils.isEmpty(mEtaInfo.getChargeStationInfo())) {
            return;
        }
        final FyChargingStation fyChargingStation = mEtaInfo.getChargeStationInfo().get(0);
        if (fyChargingStation == null || fyChargingStation.getChargeInfo() == null) {
            return;
        }
        final RouteChargeStationDetailInfo detailInfo = fyChargingStation.getChargeInfo();
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(String.format(TIP_MSG_11, detailInfo.getMName()));
        entity.setAction(MSG_ACTION_SEARCH_NEW);
        entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
        notifyUi(entity);
    }

    /***
     *电动车ETA透出---频率1分钟1次，需要实车测试
     * 注意：该接口仅在在线模式下有效，离线模式不支持
     * @param infos
     */
    public void onUpdateElectVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
        saveEnergyEndPoint(infos);
        batteryLowerTip();
    }

    /***
     * 判断是否存在能量耗尽点
     * @return 是否有能量耗尽点
     */
    private boolean hasEnergyEndPoint() {
        Logger.i(TAG, "hasEnergyEndPoint:" , (mEtaInfo != null));
        return mEtaInfo != null;
    }

    /***
     * 存储能量耗尽点对象
     * @param elecVehicleETAInfos
     */
    @SuppressLint("NewApi")
    private void saveEnergyEndPoint(final List<FyElecVehicleETAInfo> elecVehicleETAInfos) {
        if (ConvertUtils.isEmpty(elecVehicleETAInfos)) {
            mEtaInfo = null;
        } else {
            mEtaInfo = elecVehicleETAInfos.stream().filter(info -> info.isEnergyEndFlag()).findFirst().get();
        }
    }

    /***
     * 前提：有耗尽点
     * 判断当前电量还可以开多少KM
     * @return 返回剩余多少距离
     */
    private double getRemainDistance() {
        final double distance;
        final GeoPoint startPoint = mPositionPackage.currentGeo;
        final GeoPoint endPoint = mEtaInfo.getEnergyEndPoint().getShow();
        // 获取的单位是米，所以需要除 1000
        distance = mLayerPackage.calcStraightDistance(startPoint, endPoint) / 1000;
        Logger.i(TAG, "getRemainDistance:" , distance , "KM");
        return distance;
    }

    /***
     * ·预约充电站即将超时（当预约剩余时间-路程消耗时间<5分钟时触发），提示：预约充电站28分钟后超时
     * TODO 消息应该来自自营充电站
     * @param time 单位毫秒
     */
    public void appointChargeWillTimeOutTip(final long time) {
        final ChargeTipEntity entity = new ChargeTipEntity();
        final int minute = (int) (time / 1000 / 60);
        entity.setTitle(String.format(TIP_MSG_7, minute));
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        notifyUi(entity);
    }

    /***
     * 消息类型，与是否开启补能规划无关
     * ·自动降地锁提示, 预约充电桩地锁已打开
     * TODO 暂时不知道这个通知从哪里来
     */
    public void chargeUnLockerTip() {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(TIP_MSG_8);
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        notifyUi(entity);
    }

    /***
     * 前置条件：
     * 1.导航中
     * 2.目的地不可达
     * 3.剩余电量可行驶距离<=50KM
     */
    private void batteryLowerTip() {
        if (hasEnergyEndPoint() && getRemainDistance() <= EDGE_DIS) {
            Logger.i(TAG, "batteryLowerTip success!");
            final ChargeTipEntity entity = new ChargeTipEntity();
            entity.setTitle(TIP_MSG_1);
            entity.setAction(MSG_ACTION_GO_CHARGING);
            entity.setType(SceneNaviChargeBtnType.GO_CHARGING);
            notifyUi(entity);
        }
    }

    /***
     * 提示：xxx充电站目前拥挤
     */
    public void chargeBlockTip() {
        if (mViaChargeStation == null) {
            return;
        }
        if (!hasEnergyEndPoint()) {
            return;
        }
        if (ConvertUtils.isEmpty(mEtaInfo.getChargeStationInfo())) {
            return;
        }
        final FyChargingStation fyChargingStation = mEtaInfo.getChargeStationInfo().get(0);
        if (fyChargingStation.getChargeInfo() == null) {
            return;
        }
        final RouteChargeStationDetailInfo detailInfo = fyChargingStation.getChargeInfo();
        if (detailInfo == null) {
            return;
        }
        if (mViaChargeStation.time < 10 * 60 && mViaChargeStationHasFree) {
            final ChargeTipEntity entity = new ChargeTipEntity();
            entity.setTitle(String.format(TIP_MSG_2, detailInfo.getMName()));
            entity.setAction(MSG_ACTION_SEARCH_NEW);
            entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
            entity.setTtsContent(TIP_MSG_2_TTS);
            notifyUi(entity);
        }
    }

    /***
     * 提示：已为您刷新了补能规划
     */
    public void refreshChargeTip() {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(TIP_MSG_4);
        entity.setAction(MSG_ACTION_KNOW);
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        entity.setTtsContent(TIP_MSG_4);
        notifyUi(entity);
    }

    /***
     * 距离下一个充电站电量不足提示
     */
    public void arriveNextChargeStationFailedTip() {
        if (!hasEnergyEndPoint()) {
            return;
        }
        if (ConvertUtils.isEmpty(mEtaInfo.getChargeStationInfo())) {
            return;
        }
        final FyChargingStation chargingStation = mEtaInfo.getChargeStationInfo().get(0);
        if (chargingStation == null || chargingStation.getChargeInfo() == null) {
            return;
        }
        final RouteChargeStationDetailInfo detailInfo = chargingStation.getChargeInfo();
        if (detailInfo.getMRemainingCapacity() <= 0) {
            Logger.i(TAG, "arriveNextChargeStationFailedTip");
            final ChargeTipEntity entity = new ChargeTipEntity();
            final String name = detailInfo.getMName();
            entity.setTitle(String.format(TIP_MSG_5, name));
            entity.setAction(MSG_ACTION_SEARCH_NEW);
            entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
            entity.setTtsContent(String.format(TIP_MSG_5_TTS, name));
            notifyUi(entity);
        }
    }

    /***
     *
     * @param entity
     */
    private void notifyUi(final ChargeTipEntity entity) {
        Logger.i(TAG, "notifyUi", "viewModel:" , (mViewModel == null));
        if (mViewModel != null && entity != null) {
            mViewModel.notifyBatteryWarning(entity);
            if (!TextUtils.isEmpty(entity.getTtsContent())) {
                playTts(entity.getTtsContent());
            }
        }
    }

    /***
     * tts 播放提示
     * @param msg
     */
    private void playTts(final String msg) {
        Logger.i(TAG, "playTts:" , msg);
        if (mSpeechPackage != null && !ConvertUtils.isEmpty(msg)) {
            mSpeechPackage.synthesize(msg);
        }
    }

    /***
     * 测试代码
     */
    public void mockTest() {
        if (!mTestIsMocking) {
            ChargeTipEntity entity = null;
            entity = new ChargeTipEntity();
            entity.setTitle(String.format(TIP_MSG_5, "测试充电桩"));
            entity.setAction(MSG_ACTION_SEARCH_NEW);
            entity.setType(SceneNaviChargeBtnType.SEARCH_NEW_STATION);
            entity.setTtsContent(String.format(TIP_MSG_5_TTS, "测试充电桩"));
            notifyUi(entity);
            mTestIsMocking = true;
        } else {
            Logger.i(TAG, "mock 完成！");
        }
    }
}
