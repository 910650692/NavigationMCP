package com.sgm.navi.scene.impl.navi;

import android.graphics.drawable.Drawable;
import android.text.TextUtils;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.scene.ui.navi.SceneNaviSapaView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.action.Action;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ScheduledFuture;

public class SceneNaviSapaImpl extends BaseSceneModel<SceneNaviSapaView> implements SearchResultCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_SAPA_IMPL;

    private String mCurrentServicePoiId = "";
    private int mTaskId;
    private PoiInfoEntity mCurrentServicePoiInfoEntity;

    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;
    private static final int NEARLY_DISTANCE = 5000;
    //布局可见性 0:只有服务区 1:只有收费站 2:第一个是服务区 3：第一个是收费站
    public ObservableField<Integer> mViewVisible;
    // 只有服务区的名称
    public ObservableField<String> mOnlyServiceName;
    // 只有服务区的时候加油站的可见性
    public ObservableField<Boolean> mOnlyServiceGasStationVisible;
    // 只有服务区的时候餐饮的可见性
    public ObservableField<Boolean> mOnlyServiceCanteenVisible;
    // 只有服务区的时候卫生间的可见性
    public ObservableField<Boolean> mOnlyServiceLavatoryVisible;
    // 只有服务区的时候汽修的可见性
    public ObservableField<Boolean> mOnlyServiceMaintenanceVisible;
    // 只有服务区的时候购物的可见性
    public ObservableField<Boolean> mOnlyServiceBuyVisible;
    // 只有服务区的时候住宿的可见性
    public ObservableField<Boolean> mOnlyServiceHotelVisible;
    // 只有服务区的时候充电站的可见性
    public ObservableField<Boolean> mOnlyServiceChargeStationVisible;
    // 只有服务区的时候角标的显示名称
    public ObservableField<String> mOnlyServiceTag;
    // 只有服务区的时候角标的字体颜色值
    public ObservableField<Integer> mOnlyServiceTagTextColor;
    // 只有服务区的时候角标的背景颜色
    public ObservableField<Drawable> mOnlyServiceTagBgColor;
    // 只有服务区的时候剩余距离
    public ObservableField<String> mOnlyServiceDistance;
    // 只有服务区的时候慢充电桩的可用数量
    public ObservableField<String> mOnlyServiceSlowFree;
    // 只有服务区的时候快充
    public ObservableField<String> mOnlyServiceFastFree;
    // 只有收费站的名称
    public ObservableField<String> mOnlyTollName;
    // 只有收费站的etc可见性
    public ObservableField<Boolean> mOnlyTollEtcVisible;
    // 只有收费站的支付宝可见性
    public ObservableField<Boolean> mOnlyTollAlipayVisible;
    //只有收费站的时候角标的显示名称
    public ObservableField<String> mOnlyTollTag;
    // 只有收费站的时候角标的字体颜色
    public ObservableField<Integer> mOnlyTollTagTextColor;
    // 只有收费站的时候角标的背景颜色
    public ObservableField<Drawable> mOnlyTollTagBgColor;
    // 只有收费站的时候剩余距离
    public ObservableField<String> mOnlyTollDistance;
    // 第一个是服务区时服务区的名称
    public ObservableField<String> mFirstServiceName;
    // 第一个是服务区的时候加油站的可见性
    public ObservableField<Boolean> mFirstServiceGasStationVisible;
    // 第一个是服务区的时候餐饮的可见性
    public ObservableField<Boolean> mFirstServiceCanteenVisible;
    // 第一个是服务区的时候卫生间的可见性
    public ObservableField<Boolean> mFirstServiceLavatoryVisible;
    // 第一个是服务区的时候汽修的可见性
    public ObservableField<Boolean> mFirstServiceMaintenanceVisible;
    // 第一个是服务区的时候购物的可见性
    public ObservableField<Boolean> mFirstServiceBuyVisible;
    // 第一个是服务区的时候住宿的可见性
    public ObservableField<Boolean> mFirstServiceHotelVisible;
    // 第一个是服务区的时候充电站的可见性
    public ObservableField<Boolean> mFirstServiceChargeStationVisible;
    // 第一个是服务区的时候角标的显示名称
    public ObservableField<String> mFirstServiceTag;
    // 第一个是服务区的时候角标的字体颜色
    public ObservableField<Integer> mFirstServiceTagTextColor;
    // 第一个是服务区的时候角标的背景颜色
    public ObservableField<Drawable> mFirstServiceTagBgColor;
    // 第一个是服务区的时候剩余距离
    public ObservableField<String> mFirstServiceDistance;
    // 第一个是服务区的时候慢充总数量
    public ObservableField<String> mFirstServiceSlowTotal;
    // 第一个是服务区的时候慢充剩余数量
    public ObservableField<String> mFirstServiceSlowFree;
    // 第一个是服务区的时候快充总数量
    public ObservableField<String> mFirstServiceFastTotal;
    // 第一个是服务区的时候快充剩余数量
    public ObservableField<String> mFirstServiceFastFree;
    // 第一个是服务区的时候收费站的名称
    public ObservableField<String> mFirstServiceTollName;
    // 第一个是服务区的时候收费站的etc可见性
    public ObservableField<Boolean> mFirstServiceTollEtcVisible;
    // 第一个是服务区的时候收费站的支付宝可见性
    public ObservableField<Boolean> mFirstServiceTollAlipayVisible;
    //第一个是服务区的时候收费站角标显示的名称
    public ObservableField<String> mFirstServiceTollTag;
    // 第一个是服务区的时候收费站角标的字体颜色
    public ObservableField<Integer> mFirstServiceTollTagTextColor;
    // 第一个是服务区的时候收费站角标的背景颜色
    public ObservableField<Drawable> mFirstServiceTollTagBgColor;
    //第一个是服务区时收费站的剩余距离
    public ObservableField<String> mFirstServiceTollDistance;
    // 第一个是收费站时服务区的名称
    public ObservableField<String> mFirstTollServiceName;
    // 第一个是收费站时的时候加油站的可见性
    public ObservableField<Boolean> mFirstTollServiceGasStationVisible;
    // 第一个是收费站的时候餐饮的可见性
    public ObservableField<Boolean> mFirstTollServiceCanteenVisible;
    // 第一个是收费站的时候卫生间的可见性
    public ObservableField<Boolean> mFirstTollServiceLavatoryVisible;
    // 第一个是收费站的时候汽修的可见性
    public ObservableField<Boolean> mFirstTollServiceMaintenanceVisible;
    // 第一个是收费站的时候购物的可见性
    public ObservableField<Boolean> mFirstTollServiceBuyVisible;
    // 第一个是收费站的时候住宿的可见性
    public ObservableField<Boolean> mFirstTollServiceHotelVisible;
    // 第一个是收费站的时候充电站的可见性
    public ObservableField<Boolean> mFirstTollServiceChargeStationVisible;
    // 第一个是收费站的时候服务区角标的显示名称
    public ObservableField<String> mFirstTollServiceTag;
    // 第一个是收费站的时候服务区角标的字体颜色
    public ObservableField<Integer> mFirstTollServiceTagTextColor;
    // 第一个是收费站的时候服务区角标的背景颜色
    public ObservableField<Drawable> mFirstTollServiceTagBgColor;
    // 第一个是收费站时服务区剩余距离
    public ObservableField<String> mFirstTollServiceDistance;
    // 第一个是收费站的名称
    public ObservableField<String> mFirstTollName;
    // 第一个收费站的etc可见性
    public ObservableField<Boolean> mFirstTollEtcVisible;
    // 第一个是收费站的支付宝可见性
    public ObservableField<Boolean> mFirstTollAlipayVisible;
    //第一个是收费站角标显示的名称
    public ObservableField<String> mFirstTollTag;
    // 第一个是收费站角标的字体颜色
    public ObservableField<Integer> mFirstTollTagTextColor;
    // 第一个是收费站角标的背景颜色
    public ObservableField<Drawable> mFirstTollTagBgColor;
    // 第一个是收费站时的剩余距离
    public ObservableField<String> mFirstTollDistance;
    // 保存当前的sapainfo
    private SapaInfoEntity mSapaInfoEntity;
    public static final String VIA = "经";
    public static final String UN_BUILDING = "正常";
    public static final String BUILDING = "维护";
    public static final String UN_INVESTIGATION = "未知";
    public static final String REMODELING = "维护";
    public static final String TEMPORARILY_CLOSED = "关闭";
    private ISceneCallback mSceneCallback;

    public SceneNaviSapaImpl(final SceneNaviSapaView screenView) {
        super(screenView);
        mViewVisible = new ObservableField<>(0);
        mOnlyServiceName = new ObservableField<>("");
        mOnlyServiceGasStationVisible = new ObservableField<>(false);
        mOnlyServiceCanteenVisible = new ObservableField<>(false);
        mOnlyServiceLavatoryVisible = new ObservableField<>(false);
        mOnlyServiceMaintenanceVisible = new ObservableField<>(false);
        mOnlyServiceBuyVisible = new ObservableField<>(false);
        mOnlyServiceHotelVisible = new ObservableField<>(false);
        mOnlyServiceChargeStationVisible = new ObservableField<>(false);
        mOnlyServiceTag = new ObservableField<>("");
        mOnlyServiceTagTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getColor(R.color.navi_color_FFFFFF_100));
        mOnlyServiceTagBgColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_navi_via_item_btn_pass));
        mOnlyServiceDistance = new ObservableField<>("");
        mOnlyServiceSlowFree = new ObservableField<>("");
        mOnlyServiceFastFree = new ObservableField<>("");
        mOnlyTollName = new ObservableField<>("");
        mOnlyTollEtcVisible = new ObservableField<>(false);
        mOnlyTollAlipayVisible = new ObservableField<>(false);
        mOnlyTollTag = new ObservableField<>("");
        mOnlyTollTagTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getColor(R.color.navi_color_FFFFFF_100));
        mOnlyTollTagBgColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_navi_via_item_btn_pass));
        mOnlyTollDistance = new ObservableField<>("");
        mFirstServiceName = new ObservableField<>("");
        mFirstServiceGasStationVisible = new ObservableField<>(false);
        mFirstServiceCanteenVisible = new ObservableField<>(false);
        mFirstServiceLavatoryVisible = new ObservableField<>(false);
        mFirstServiceMaintenanceVisible = new ObservableField<>(false);
        mFirstServiceBuyVisible = new ObservableField<>(false);
        mFirstServiceHotelVisible = new ObservableField<>(false);
        mFirstServiceChargeStationVisible = new ObservableField<>(false);
        mFirstServiceTag = new ObservableField<>("");
        mFirstServiceTagTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getColor(R.color.navi_color_FFFFFF_100));
        mFirstServiceTagBgColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_navi_via_item_btn_pass));
        mFirstServiceDistance = new ObservableField<>("");
        mFirstServiceFastTotal = new ObservableField<>("");
        mFirstServiceFastFree = new ObservableField<>("");
        mFirstServiceSlowFree = new ObservableField<>("");
        mFirstServiceSlowTotal = new ObservableField<>("");
        mFirstServiceTollName = new ObservableField<>("");
        mFirstServiceTollEtcVisible = new ObservableField<>(false);
        mFirstServiceTollAlipayVisible = new ObservableField<>(false);
        mFirstServiceTollTag = new ObservableField<>("");
        mFirstServiceTollTagTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getColor(R.color.navi_color_FFFFFF_100));
        mFirstServiceTollTagBgColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_navi_via_item_btn_pass));
        mFirstServiceTollDistance = new ObservableField<>("");
        mFirstTollName = new ObservableField<>("");
        mFirstTollEtcVisible = new ObservableField<>(false);
        mFirstTollAlipayVisible = new ObservableField<>(false);
        mFirstTollTag = new ObservableField<>("");
        mFirstTollTagTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getColor(R.color.navi_color_FFFFFF_100));
        mFirstTollTagBgColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_navi_via_item_btn_pass));
        mFirstTollDistance = new ObservableField<>("");
        mFirstTollServiceName = new ObservableField<>("");
        mFirstTollServiceGasStationVisible = new ObservableField<>(false);
        mFirstTollServiceCanteenVisible = new ObservableField<>(false);
        mFirstTollServiceLavatoryVisible = new ObservableField<>(false);
        mFirstTollServiceMaintenanceVisible = new ObservableField<>(false);
        mFirstTollServiceBuyVisible = new ObservableField<>(false);
        mFirstTollServiceHotelVisible = new ObservableField<>(false);
        mFirstTollServiceChargeStationVisible = new ObservableField<>(false);
        mFirstTollServiceTag = new ObservableField<>("");
        mFirstTollServiceTagTextColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getColor(R.color.navi_color_FFFFFF_100));
        mFirstTollServiceTagBgColor = new ObservableField<>(ResourceUtils.Companion.getInstance().
                getDrawable(R.drawable.img_navi_via_item_btn_pass));
        mFirstTollServiceDistance = new ObservableField<>("");
    }

    public Action rootViewClick = () -> Logger.i(TAG, "点击事件拦截");

    @Override
    protected void onCreate() {
        super.onCreate();
        OpenApiHelper.registerSearchResultCallback(TAG, this);
    }

    /**
     * @param sapaInfoEntity SAPA信息
     */
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        Logger.i(TAG, "SceneNaviSAPAImpl onNaviSAPAInfo type: ", sapaInfoEntity.toString());
        mSapaInfoEntity = sapaInfoEntity;
        switch (sapaInfoEntity.getType()) {
            // 服务区/收费站为0
            case NaviConstant.SapaItemsType.AUTO_UNKNOWN_ERROR:
                updateSceneVisible(false);
                break;
            case NaviConstant.SapaItemsType.SPAS_LIST://服务区
                if (!ConvertUtils.isEmpty(sapaInfoEntity.getList())) {
                    SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(0);
                    String servicePoiId = sapaItem.getServicePOIID();
                    servicePoiIdSearch(servicePoiId);
                    // 显示只有服务区的布局
                    mViewVisible.set(0);
                    updateOnlyServiceData(sapaItem);
                }
                break;
            case NaviConstant.SapaItemsType.TOLL_STATION_LIST://收费站
                if (!ConvertUtils.isEmpty(sapaInfoEntity.getList())) {
                    // 显示只有收费站的布局
                    mViewVisible.set(1);
                    updateOnlyTollData(sapaInfoEntity.getList().get(0),
                            sapaInfoEntity.getLaneTypes());
                    cancelTimer();
                    mScreenView.resetUi();
                }
                break;
            case NaviConstant.SapaItemsType.TOLL_STATION_AND_SPAS://一个服务区，一个收费站
                if (!ConvertUtils.isEmpty(sapaInfoEntity.getList())) {
                    final SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(0);
                    int distance = sapaItem.getRemainDist();
                    Logger.i(TAG, "SceneNaviSAPAImpl TOLL_STATION_AND_SPAS ",
                            "onNaviSAPAInfo distance: ", distance);
                    // 设置临近态判断，如果距离小于5公里，则显示临近态
                    boolean showNearly = distance <= NEARLY_DISTANCE;
                    if (sapaItem.getType() == NaviConstant.SapaItemsType.SPAS_LIST) {//第一个是服务区，第二个是收费站
                        String servicePoiId = sapaItem.getServicePOIID();
                        servicePoiIdSearch(servicePoiId);
                        if (showNearly) {
                            mViewVisible.set(0);
                            updateOnlyServiceData(sapaItem);
                        } else {
                            mViewVisible.set(2);
                            updateFirstServiceData(sapaItem, sapaInfoEntity.getList().get(1),
                                    sapaInfoEntity.getLaneTypes());
                        }
                    } else {//第一个是收费站，第二个是服务区
                        cancelTimer();
                        mScreenView.resetUi();
                        if (showNearly) {
                            mViewVisible.set(1);
                            updateOnlyTollData(sapaItem, sapaInfoEntity.getLaneTypes());
                        } else {
                            mViewVisible.set(3);
                            updateFirstTollData(sapaItem, sapaInfoEntity.getList().get(1),
                                    sapaInfoEntity.getLaneTypes());
                        }
                    }
                }
                break;
            default:
                resetSapa();
                break;
        }
    }

    private void servicePoiIdSearch(String poiId) {
        Logger.i(TAG, "currentServicePoiId = ", mCurrentServicePoiId, " poiId = ", poiId);
        int powerType = OpenApiHelper.powerType();
        // 电车和插混的才搜索显示充电桩的信息
        if (powerType == 1 || powerType == 2) {
            if (!mCurrentServicePoiId.equals(poiId)) {
                mTaskId = OpenApiHelper.poiIdSearch(poiId);
                Logger.i(TAG, "search servicePoiID taskId = ", mTaskId);
                mCurrentServicePoiId = poiId;
            }
        }

    }

    @Override
    protected void onDestroy() {
        resetSapa();
        super.onDestroy();
        OpenApiHelper.unRegisterSearchResultCallback(TAG);
        cancelTimer();
    }

    /**
     * 重置sapa
     */
    private void resetSapa() {
        updateSceneVisible(false);
    }

    /**
     * @param sceneCallback 回调接口
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mSceneCallback = sceneCallback;
    }

    /**
     * @param isVisible 是否显示
     */
    private void updateSceneVisible(final boolean isVisible){
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(TAG, "SceneNaviSAPAImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SCENE_SERVICE_AREA);
    }

    /**
     * @param sapItem 服务区更新需要的数据
     * @param sapItemSecond 收费站更新需要的数据
     * @param laneTypes     收费站的车道类型
     */
    public void updateFirstServiceData(final SapaInfoEntity.SAPAItem sapItem,
                                       final SapaInfoEntity.SAPAItem sapItemSecond,
                                       final ArrayList<Integer> laneTypes) {
        if (TextUtils.isEmpty(sapItem.getName())) {
            // 信息是空的可以sdk返回的是空数据，隐藏页面显示
            updateSceneVisible(false);
            return;
        }
        //服务区名称
        mFirstServiceName.set(sapItem.getName());
        updateServiceDetails(sapItem, mFirstServiceGasStationVisible, mFirstServiceCanteenVisible,
                mFirstServiceLavatoryVisible, mFirstServiceMaintenanceVisible, mFirstServiceBuyVisible,
                mFirstServiceHotelVisible, mFirstServiceChargeStationVisible);
        tagUpdate(mFirstServiceTag, mFirstServiceTagTextColor, mFirstServiceTagBgColor, sapItem);
        updateDistance(sapItem, mFirstServiceDistance);
        //收费站名称
        mFirstServiceTollName.set(sapItemSecond.getName());
        updateTollDetail(laneTypes, mFirstServiceTollEtcVisible, mFirstServiceTollAlipayVisible);
        tagUpdate(mFirstServiceTollTag, mFirstServiceTollTagTextColor, mFirstServiceTollTagBgColor,
                sapItemSecond);
        updateDistance(sapItemSecond, mFirstServiceTollDistance);
        updateSceneVisible(true);
    }

    /**
     * @param sapItem 收费站更新需要的数据
     * @param sapItemSecond 服务区更新需要的数据
     * @param laneTypes      收费站的车道类型
     */
    public void updateFirstTollData(final SapaInfoEntity.SAPAItem sapItem,
                                    final SapaInfoEntity.SAPAItem sapItemSecond,
                                    final ArrayList<Integer> laneTypes) {
        if (TextUtils.isEmpty(sapItem.getName())) {
            // 信息是空的可以sdk返回的是空数据，隐藏页面显示
            updateSceneVisible(false);
            return;
        }
        //收费站名称
        mFirstTollName.set(sapItem.getName());
        updateTollDetail(laneTypes, mFirstTollEtcVisible, mFirstTollAlipayVisible);
        tagUpdate(mFirstTollTag, mFirstTollTagTextColor, mFirstTollTagBgColor, sapItem);
        updateDistance(sapItem, mFirstTollDistance);
        //服务区名称
        mFirstTollServiceName.set(sapItemSecond.getName());
        updateServiceDetails(sapItemSecond, mFirstTollServiceGasStationVisible, mFirstTollServiceCanteenVisible,
                mFirstTollServiceLavatoryVisible, mFirstTollServiceMaintenanceVisible, mFirstTollServiceBuyVisible,
                mFirstTollServiceHotelVisible, mFirstTollServiceChargeStationVisible);
        tagUpdate(mFirstTollServiceTag, mFirstTollServiceTagTextColor, mFirstTollServiceTagBgColor,
                sapItemSecond);
        updateDistance(sapItemSecond, mFirstTollServiceDistance);
        updateSceneVisible(true);
    }

    /**
     * @param sapItem 更新只有服务区的数据
     */
    public void updateOnlyServiceData(final SapaInfoEntity.SAPAItem sapItem) {
        if (TextUtils.isEmpty(sapItem.getName())) {
            // 信息是空的可以sdk返回的是空数据，隐藏页面显示
            updateSceneVisible(false);
            return;
        }
        //服务区名称
        mOnlyServiceName.set(sapItem.getName());
        updateServiceDetails(sapItem, mOnlyServiceGasStationVisible, mOnlyServiceCanteenVisible,
                mOnlyServiceLavatoryVisible, mOnlyServiceMaintenanceVisible, mOnlyServiceBuyVisible,
                mOnlyServiceHotelVisible, mOnlyServiceChargeStationVisible);
        tagUpdate(mOnlyServiceTag, mOnlyServiceTagTextColor, mOnlyServiceTagBgColor, sapItem);
        updateDistance(sapItem, mOnlyServiceDistance);
        updateSceneVisible(true);
    }

    /**
     * 更新只有收费站的数据
     * @param sapItem   sapa
     * @param laneTypes laneTypes
     */
    public void updateOnlyTollData(final SapaInfoEntity.SAPAItem sapItem,
                                   final ArrayList<Integer> laneTypes) {
        if (TextUtils.isEmpty(sapItem.getName())) {
            // 信息是空的可以sdk返回的是空数据，隐藏页面显示
            updateSceneVisible(false);
            return;
        }
        mOnlyTollName.set(sapItem.getName());
        updateTollDetail(laneTypes, mOnlyTollEtcVisible, mOnlyTollAlipayVisible);
        tagUpdate(mOnlyTollTag, mOnlyTollTagTextColor, mOnlyTollTagBgColor, sapItem);
        updateDistance(sapItem, mOnlyTollDistance);
        updateSceneVisible(true);
    }

    /**
     * 更新可收费类型详情
     * @param laneTypes 车道收费类型
     * @param etc etc标识
     * @param alipay 支付宝标识
     */
    private void updateTollDetail(final ArrayList<Integer> laneTypes,
                                  final ObservableField<Boolean> etc,
                                  final ObservableField<Boolean> alipay) {
        etc.set(false);
        alipay.set(false);
        //Crash
        if (ConvertUtils.isEmpty(laneTypes)) {
            return;
        }
        for (Integer integer : laneTypes) {
            // TollLaneTypeETC等于2可以显示etc标识
            if (integer == 2) {
                etc.set(true);
            }
            // TollLaneTypeAliPay等于8可以显示支付宝标识
            if (integer == 8) {
                alipay.set(true);
            }
        }
    }

    /**
     * 更新剩余距离信息
     * @param sapItem    sapItem
     * @param observable observable
     */
    private void updateDistance(final SapaInfoEntity.SAPAItem sapItem,
                                final ObservableField<String> observable) {
        final int distance = sapItem.getRemainDist();
        observable.set(DataHelper.convertMetersToKilometers(distance));
    }



    /**
     * 更新服务区详情
     * @param sapItem     sapItem
     * @param gasStation gasStation
     * @param canteen    canteen
     * @param lavatory   lavatory
     * @param maintenance maintenance
     * @param buy        buy
     * @param hotel       hotel
     */
    private void updateServiceDetails(final SapaInfoEntity.SAPAItem sapItem,
                                      final ObservableField<Boolean> gasStation,
                                      final ObservableField<Boolean> canteen,
                                      final ObservableField<Boolean> lavatory,
                                      final ObservableField<Boolean> maintenance,
                                      final ObservableField<Boolean> buy,
                                      final ObservableField<Boolean> hotel,
                                      final ObservableField<Boolean> chargeStation) {
        // 服务区详情
        final long sapaDetail = sapItem.getSapaDetail();
        Logger.i(TAG, "updateServiceDetails: sapaDetail = ", sapaDetail);
        int totalDetails = 0;
        int powerType = OpenApiHelper.powerType();
        for (int i = 0; i <= 6; i++) {
            final int nthBit = DataHelper.getNthBit(sapaDetail, i);
            if (nthBit == 1) {
                totalDetails ++;
            }
            switch (i) {
                case 0:
                    if (powerType == 0 || powerType == 2) {
                        gasStation.set(nthBit == 1);
                    }
                    break;
                case 1:
                    canteen.set(nthBit == 1);
                    break;
                case 2:
                    lavatory.set(nthBit == 1);
                    break;
                case 3:
                    maintenance.set(nthBit == 1);
                    break;
                case 4:
                    buy.set(nthBit == 1);
                    break;
                case 5:
                    hotel.set(nthBit == 1);
                    break;
                case 6:
                    if (powerType == 2 || powerType == 1) {
                        chargeStation.set(nthBit == 1);
                    }
                    break;
                default:
                    break;
            }
        }
        // UI碰撞问题存在和浦春沟通过，如果显示不下就不显示，在详情中再显示完全
        if (totalDetails == 7 && hotel != mOnlyServiceHotelVisible) {
            hotel.set(false);
        }
    }

    /**
     * 更新标签
     * @param tag     tag
     * @param sapItem sapItem
     */
    private void tagUpdate(final ObservableField<String> tag,
                           final ObservableField<Integer> tagTextColor,
                           final ObservableField<Drawable> tagBgColor,
                           final SapaInfoEntity.SAPAItem sapItem) {
        // 角标显示 途经点>维护/关闭类>其他
        // 判断是否是途经点
        final List<RouteParam> allPoiParamList = RoutePackage.getInstance().getAllPoiParamList(mMapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (Objects.equals(routeParam.getPoiID(), sapItem.getServicePOIID())) {
                tag.set(VIA);
                tagTextColor.set(ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_FFFFFF_100));
                tagBgColor.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_navi_via_item_btn_pass));
                return;
            }
            boolean isCanJudgeGps = null != routeParam.getRealPos() && null != sapItem.getPos();
            if (isCanJudgeGps) {
                if (routeParam.getRealPos().getLat() == sapItem.getPos().getLat() && routeParam.getRealPos().getLon() == sapItem.getPos().getLon()) {
                    tag.set(VIA);
                    tagTextColor.set(ResourceUtils.Companion.getInstance().getColor(R.color.navi_color_FFFFFF_100));
                    tagBgColor.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_navi_via_item_btn_pass));
                    return;
                }
            }
        }
        //服务区状态:0 正常（默认值），1 建设中，2 未调查 3 装修中 4 暂停营业
        final int buildingStatus = sapItem.getBuildingStatus();
        switch (buildingStatus) {
            case 0:
                tag.set(UN_BUILDING);
                break;
            case 1:
                tag.set(BUILDING);
                break;
            case 3:
                tag.set(REMODELING);
                break;
            case 4:
                tag.set(TEMPORARILY_CLOSED);
                break;
            default:
                tag.set(UN_INVESTIGATION);
                break;
        }
        tagTextColor.set(ResourceUtils.Companion.getInstance().getColor(R.color.black_always));
        tagBgColor.set(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_navi_via_item_btn_normal));
    }

    /**
     * @param type type
     */
    public void onClick(final int type) {
        Logger.d(TAG, "onClick: type = ", type);
        if (!TimerHelper.isCanDo()) {
            return;
        }
        if (mSapaInfoEntity == null) {
            Logger.e(TAG, "onClick: mSapaInfoEntity is null");
            return;
        }
        if (SceneNaviSapaDetailImpl.SERVICE_DETAIL_PAGE == type) {
            mSceneCallback.skipNaviSapaDetailScene(SceneNaviSapaDetailImpl.SERVICE_DETAIL_PAGE,
                    mSapaInfoEntity);
        } else {
            mSceneCallback.skipNaviSapaDetailScene(SceneNaviSapaDetailImpl.TOLL_DETAIL_PAGE,
                    mSapaInfoEntity);
        }
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message,
                               final SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSearchResult: taskId = ", taskId, ", errorCode = ", errorCode,
                ", message = ", message);
        if (mTaskId == taskId) {
            if (null != searchResultEntity) {
                List<PoiInfoEntity> poiInfoEntityList = searchResultEntity.getPoiList();
                for (PoiInfoEntity poiInfoEntity : poiInfoEntityList) {
                    if (Objects.equals(mCurrentServicePoiId, poiInfoEntity.getMPid())) {
                        mCurrentServicePoiInfoEntity = poiInfoEntity;
                        List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
                        int fastFree = 0;
                        int fastTotal = 0;
                        int slowFree = 0;
                        int slowTotal = 0;
                        for (ChargeInfo chargeInfo : chargeInfoList) {
                            fastFree = fastFree + chargeInfo.getFast_free();
                            fastTotal = fastTotal + chargeInfo.getFast_total();
                            slowFree = slowFree + chargeInfo.getSlow_free();
                            slowTotal = slowTotal + chargeInfo.getSlow_total();
                        }
                        if (fastTotal != 0 || fastFree != 0 || slowFree != 0 || slowTotal != 0) {
                            mFirstServiceFastFree.set("" + fastFree);
                            mFirstServiceFastTotal.set("/" + fastTotal);
                            mFirstServiceSlowFree.set("" + slowFree);
                            mFirstServiceSlowTotal.set("/" + slowTotal);
                            mOnlyServiceFastFree.set("" + fastFree);
                            mOnlyServiceSlowFree.set("" + slowFree);
                            initTimer();
                        } else {
                            cancelTimer();
                        }
                    }
                }
            }
        }
    }

    /**
     * 开始倒计时
     */
    public void initTimer() {
        Logger.i(TAG, "initTimer");
        cancelTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(mRefreshChargeInfo);
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    private Runnable mRefreshChargeInfo = new Runnable() {
        @Override
        public void run() {
            Logger.i(TAG, "mRefreshChargeInfo");
            if (mViewVisible.get() == 0) {
                mScreenView.updateOnlyServiceUi();
            } else if (mViewVisible.get() == 2) {
                mScreenView.updateFirstServiceUi();
            }
        }
    };

    /**
     * 取消倒计时
     */
    public void cancelTimer() {
        Logger.i(TAG, "cancelTimer");
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }
}
