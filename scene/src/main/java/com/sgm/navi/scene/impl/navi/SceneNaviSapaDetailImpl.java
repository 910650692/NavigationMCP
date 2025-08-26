package com.sgm.navi.scene.impl.navi;

import android.text.TextUtils;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.ui.navi.SceneNaviSapaDetailView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.AdminCodeBean;
import com.sgm.navi.service.define.bean.AreaExtraInfoBean;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.ui.action.Action;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SceneNaviSapaDetailImpl extends BaseSceneModel<SceneNaviSapaDetailView> implements
        SearchResultCallback {
    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_SAPA_DETAIL_IMPL;

    //0:显示服务区详情页，1：显示收费站详情页
    public ObservableField<Integer> mViewType;
    //服务区详情页的状态标签
    public ObservableField<String> mServiceStatusTag;
    //服务区名称
    public ObservableField<String> mServiceName;
    //服务区距离
    public ObservableField<String> mServiceDistance;
    //到服务区剩余时间
    public ObservableField<String> mServiceRemainTime;
    //服务区道路名称
    public ObservableField<String> mServiceRouteName;
    //服务区充电站的可见性
    public ObservableField<Boolean> mServiceChargeStationVisible;
    //服务区餐厅的可见性
    public ObservableField<Boolean> mServiceCanteenVisible;
    //服务区维修站的可见性
    public ObservableField<Boolean> mServiceMaintenanceVisible;
    //服务区厕所的可见性
    public ObservableField<Boolean> mServiceLavatoryVisible;
    //服务区购物的可见性
    public ObservableField<Boolean> mServiceBuyVisible;
    //服务区酒店的可见性
    public ObservableField<Boolean> mServiceHotelVisible;
    //服务区加油站背景的可见性
    public ObservableField<Boolean> mServiceGasStationVisible;
    //服务区98号汽油的可见性
    public ObservableField<Boolean> mServiceGasType98Visible;
    //服务区95号汽油的可见性
    public ObservableField<Boolean> mServiceGasType95Visible;
    //服务区92号汽油的可见性
    public ObservableField<Boolean> mServiceGasType92Visible;
    //服务区0号汽油的可见性
    public ObservableField<Boolean> mServiceGasType0Visible;
    //服务区营业时间
    public ObservableField<String> mServiceBusinessTime;
    //服务区电话
    public ObservableField<String> mServiceTel;
    //服务区背景的可见性
    public ObservableField<Boolean> mServicesVisible;
    //收费站详情页的状态标签
    public ObservableField<String> mTollStatusTag;
    //收费站名称
    public ObservableField<String> mTollName;
    //收费站距离
    public ObservableField<String> mTollDistance;
    //到收费站剩余时间
    public ObservableField<String> mTollRemainTime;
    //收费站道路名称
    public ObservableField<String> mTollRouteName;
    //收费站支付宝的可见性
    public ObservableField<Boolean> mTollAlipayVisible;
    //收费站etc的可见性
    public ObservableField<Boolean> mTollEtcVisible;
    //收费站营业时间
    public ObservableField<String> mTollBusinessTime;
    //服务区电话
    public ObservableField<String> mTollTel;
    // 收费站服务背景的可见性
    public ObservableField<Boolean> mTollServicesVisible;
    // 显示的按钮文字 删除途经点还是添加途经点
    public ObservableField<String> mButtonText;
    public static final int SERVICE_DETAIL_PAGE = 0;
    public static final int TOLL_DETAIL_PAGE = 1;

    private int mSapaSearchId = -1;
    private int mSapaGeoSearchId = -1;
    private int mTollGeoSearchId = -1;

    // 当前的poi信息 （用于添加途经点）
    private PoiInfoEntity mCurrentPoiInfoEntity;
    // 当前的poi是否是途经点
    private boolean mIsVia;
    // 当前的poi类型（用于添加途经点）
    private int mCurrentPoiType = -1;
    private int powerType;
    private Runnable mRefreshInfoRunnable;
    private Runnable mRefreshGeoRunnable;

    public SceneNaviSapaDetailImpl(final SceneNaviSapaDetailView screenView) {
        super(screenView);
        mViewType = new ObservableField<>(0);
        mServiceStatusTag = new ObservableField<>("");
        mServiceName = new ObservableField<>("");
        mServiceDistance = new ObservableField<>("");
        mServiceRemainTime = new ObservableField<>("");
        mServiceRouteName = new ObservableField<>("");
        mServiceChargeStationVisible = new ObservableField<>(false);
        mServiceCanteenVisible = new ObservableField<>(false);
        mServiceMaintenanceVisible = new ObservableField<>(false);
        mServiceLavatoryVisible = new ObservableField<>(false);
        mServiceBuyVisible = new ObservableField<>(false);
        mServiceHotelVisible = new ObservableField<>(false);
        mServiceGasType98Visible = new ObservableField<>(false);
        mServiceGasType95Visible = new ObservableField<>(false);
        mServiceGasType92Visible = new ObservableField<>(false);
        mServiceGasType0Visible = new ObservableField<>(false);
        mServiceBusinessTime = new ObservableField<>("");
        mServiceTel = new ObservableField<>("");
        mTollStatusTag = new ObservableField<>("");
        mTollName = new ObservableField<>("");
        mTollDistance = new ObservableField<>("");
        mTollRemainTime = new ObservableField<>("");
        mTollRouteName = new ObservableField<>("");
        mTollAlipayVisible = new ObservableField<>(false);
        mTollEtcVisible = new ObservableField<>(false);
        mTollBusinessTime = new ObservableField<>("");
        mTollTel = new ObservableField<>("");
        mServicesVisible = new ObservableField<>(false);
        mTollServicesVisible = new ObservableField<>(false);
        mServiceGasStationVisible = new ObservableField<>(false);
        mButtonText = new ObservableField<>();
        powerType = OpenApiHelper.powerType();
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        SearchPackage.getInstance().registerCallBack("SceneNaviSapaDetailImpl",
                this);
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ThreadManager.getInstance().removeHandleTask(mRefreshInfoRunnable);
        ThreadManager.getInstance().removeHandleTask(mRefreshGeoRunnable);
        SearchPackage.getInstance().unRegisterCallBack("SceneNaviSapaDetailImpl");
    }

    /**
     * @param isVisible 是否显示
     */
    private void updateSceneVisible(final boolean isVisible, boolean isReset) {
        if (mScreenView.isVisible() == isVisible) return;
        Logger.i(TAG, "SceneNaviSapaDetailImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChangeReset(
                (isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                        INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SAPA_DETAIL_INFO, isReset);
    }

    /**
     * 处理详情页显示
     *
     * @param type           类型
     * @param sapaInfoEntity 实体类
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        Logger.i(TAG, "skipNaviSapaDetailScene", ", type = ", type);
        if (sapaInfoEntity == null) {
            Logger.e(TAG, "sapaInfoEntity is null");
            return;
        }
        ArrayList<SapaInfoEntity.SAPAItem> list = sapaInfoEntity.getList();
        if (!ConvertUtils.isEmpty(list)) {
            if (SERVICE_DETAIL_PAGE == type) {
                SapaInfoEntity.SAPAItem sapaItem = list.get(0);
                if (sapaItem.getType() != 0 && list.size() > 1) {
                    sapaItem = sapaInfoEntity.getList().get(1);
                }
                if (sapaItem != null) {
                    doServiceSearch(sapaItem);
                    mViewType.set(0);
                    updateServiceDetailInfo(sapaItem);
                } else {
                    Logger.e(TAG, "sapaItem is null");
                }
            } else {
                SapaInfoEntity.SAPAItem sapaItem = list.get(0);
                if (sapaItem.getType() != 1 && list.size() > 1) {
                    sapaItem = list.get(1);
                }
                if (sapaItem != null) {
                    doTollSearch(sapaItem);
                    mViewType.set(1);
                    updateTollDetailInfo(sapaItem, sapaInfoEntity.getLaneTypes());
                } else {
                    Logger.e(TAG, "sapaItem is null");
                }
            }
            updateSceneVisible(true, false);
        }
    }

    /**
     * 收费站透传动作，因为SapaInfoEntity中不会透出收费站的poiId,手动生成poiEntityInfo
     *
     * @param sapaItem 实体类
     */
    private void doTollSearch(final SapaInfoEntity.SAPAItem sapaItem) {
        if (sapaItem != null) {
            PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
            poiInfoEntity.setMName(sapaItem.getName());
            poiInfoEntity.setMPoint(sapaItem.getPos());
            poiInfoEntity.setPoiType(16);
            poiInfoEntity.setPoiTag(ResourceUtils.Companion.getInstance().getString(R.string.toll));
            mCurrentPoiInfoEntity = poiInfoEntity;
            mCurrentPoiType = 16;
            if (!ConvertUtils.isEmpty(sapaItem.getPos())) {
                mTollGeoSearchId = SearchPackage.getInstance().geoSearch(sapaItem.getPos(), true);
                Logger.d(TAG, "mTollGeoSearchId = ", mTollGeoSearchId, " Name:", sapaItem.getName());
            } else {
                Logger.i(TAG, "sapaItem.getPos() is empty! Name:", sapaItem.getName());
            }
        } else {
            Logger.e(TAG, "doTollSearch sapaItem is null");
        }
    }

    /**
     * 服务区搜索动作
     *
     * @param sapaItem 实体类
     */
    private void doServiceSearch(final SapaInfoEntity.SAPAItem sapaItem) {
        if (sapaItem != null) {
            final String poiId = sapaItem.getServicePOIID();
            if (!ConvertUtils.isEmpty(sapaItem.getPos())) {
                mSapaGeoSearchId = SearchPackage.getInstance().geoSearch(sapaItem.getPos(), true);
                Logger.d(TAG, "mSapaGeoSearchId = ", mSapaGeoSearchId, " Name:", sapaItem.getName());
            } else {
                Logger.i(TAG, "sapaItem.getPos() is empty! Name:", sapaItem.getName());
            }
            Logger.i(TAG, poiId);
            if (!TextUtils.isEmpty(poiId) && !poiId.contains(".") && poiId.startsWith("B")) {
                // 进行PoiId搜索 为了途经点添加功能
                mSapaSearchId = SearchPackage.getInstance().poiIdSearch(poiId, true);
                Logger.d(TAG, " mSapaSearchId = ", mSapaSearchId, " Name:", sapaItem.getName());
            } else if (!ConvertUtils.isNull(sapaItem.getPos())){
                mSapaSearchId = SearchPackage.getInstance().geoSearch(sapaItem.getPos(), true);
                Logger.d(TAG, " mSapaSearchId = ", mSapaSearchId, " Name:", sapaItem.getName());
            } else {
                Logger.e(TAG, "poiId无效，地址为空，无法进行搜索");
            }
        } else {
            Logger.e(TAG, "sapaItem is null");
        }
    }

    /**
     * 刷新服务区加油站信息
     *
     * @param result poi点详情信息
     */
    private void refreshGasStationInfo(final SearchResultEntity result) {
        if (result == null) {
            Logger.i(TAG, "refreshGasStationInfo result is null");
            return;
        }
        mServiceGasStationVisible.set(false);
        mServiceGasType92Visible.set(false);
        mServiceGasType95Visible.set(false);
        mServiceGasType98Visible.set(false);
        mServiceGasType0Visible.set(false);
        final List<PoiInfoEntity> poiList = result.getPoiList();
        PoiInfoEntity poiInfo = null;
        if (!ConvertUtils.isEmpty(poiList)) {
            poiInfo = poiList.get(0);
        }
        if (poiInfo == null) {
            Logger.i(TAG, "refreshGasStationInfo poiInfo is null");
            return;
        }
        final List<GasStationInfo> stationList = poiInfo.getStationList();
        if (ConvertUtils.isEmpty(stationList)) {
            Logger.i(TAG, "refreshGasStationInfo stationList is null");
            return;
        }
        for (GasStationInfo stationInfo : stationList) {
            if (stationInfo == null || stationInfo.getType() == null) {
                continue;
            }
            if (stationInfo.getType().contains("98")) {
                mServiceGasType98Visible.set(true);
            }
            if (stationInfo.getType().contains("95")) {
                mServiceGasType95Visible.set(true);
            }
            if (stationInfo.getType().contains("92")) {
                mServiceGasType92Visible.set(true);
            }
            if (stationInfo.getType().contains("0")) {
                mServiceGasType0Visible.set(true);
            }
        }

        if (Boolean.TRUE.equals(mServiceGasType98Visible.get()) ||
                Boolean.TRUE.equals(mServiceGasType95Visible.get()) ||
                Boolean.TRUE.equals(mServiceGasType92Visible.get()) ||
                Boolean.TRUE.equals(mServiceGasType0Visible.get())) {
            int powerType = OpenApiHelper.powerType();
            // 油车和插混才显示加油站信息
            if (powerType == 0 || powerType == 2) {
                mServiceGasStationVisible.set(true);
            }
        }
    }

    private void refreshGeoStationInfo(final SearchResultEntity result) {
        if (result == null) {
            Logger.i(TAG, "refreshStationInfo result is null");
            return;
        }
        final List<PoiInfoEntity> poiList = result.getPoiList();
        PoiInfoEntity poiInfo = null;
        if (!ConvertUtils.isEmpty(poiList)) {
            poiInfo = poiList.get(0);
        }
        if (poiInfo == null) {
            Logger.i(TAG, "refreshStationInfo poiInfo is null");
            return;
        }
        String address = "";
        if (poiInfo.getAdCode() > 0) {
            address = getAreaName(poiInfo.getAdCode());
        }
        if (ConvertUtils.isEmpty(address)) {
            address = poiInfo.getAddress();
        }
        if (ConvertUtils.isEmpty(address)) {
            address = poiInfo.getName();
        }
        Logger.d(TAG, " getAddress = ", poiInfo.getAddress(), " getAdCode = ",
                poiInfo.getAdCode(), " address = ", address);
        mServiceRouteName.set(address);
    }


    /**
     * @param sapaItem 更新服务区详情页的信息
     */
    private void updateServiceDetailInfo(final SapaInfoEntity.SAPAItem sapaItem) {
        if (sapaItem == null) {
            Logger.i(TAG, "updateServiceDetailInfo sapItem is null");
            return;
        }
        if (powerType == 1) {
            GeoPoint point = sapaItem.getPos();
            OpenApiHelper.getTravelTimeFutureIncludeChargeLeft(point).thenAccept(etaInfo -> {
                ThreadManager.getInstance().postUi(() -> {
                    int leftCharge = etaInfo.getLeftCharge();
                    mScreenView.updateServiceChargeUi(leftCharge);
                });
            }).exceptionally(error -> {
                Logger.e(TAG, "updateServiceDetailInfo getTravelTimeFuture error:", error);
                return null;
            });
        }
        mServiceName.set(sapaItem.getName());
        tagUpdate(mServiceStatusTag, sapaItem);
        updateDistanceAndRemainTime(sapaItem, mServiceDistance, mServiceRemainTime);
        updateServiceDetails(sapaItem, mServiceChargeStationVisible, mServiceCanteenVisible,
                mServiceLavatoryVisible, mServiceMaintenanceVisible, mServiceBuyVisible,
                mServiceHotelVisible);
        /*String areaName = getAreaName();
        mServiceRouteName.set(areaName);*/
    }


    /**
     * 更新收费站地址
     *
     * @param result 搜索结果
     */
    private void refreshTollInfo(final SearchResultEntity result, boolean isGeoSearch) {
        if (result == null) {
            Logger.i(TAG, "refreshTollInfo result is null isGeoSearch:", isGeoSearch);
            return;
        }
        final List<PoiInfoEntity> poiList = result.getPoiList();
        PoiInfoEntity poiInfo = null;
        if (!ConvertUtils.isEmpty(poiList)) {
            poiInfo = poiList.get(0);
        }
        if (poiInfo == null) {
            Logger.i(TAG, "refreshTollInfo poiInfo is null isGeoSearch:", isGeoSearch);
            return;
        }
        String address = "";
        if (poiInfo.getAdCode() > 0) {
            address = getAreaName(poiInfo.getAdCode());
        }
        if (ConvertUtils.isEmpty(address)) {
            address = poiInfo.getAddress();
        }
        if (ConvertUtils.isEmpty(address)) {
            address = poiInfo.getName();
        }
        Logger.d(TAG, " isGeoSearch:", isGeoSearch, " getAddress = ", poiInfo.getAddress(), " getAdCode = ",
                poiInfo.getAdCode(), " address = ", address);
        mTollRouteName.set(address);
    }

    /**
     * @param sapaItem 更新收费站详情页的信息
     */
    private void updateTollDetailInfo(final SapaInfoEntity.SAPAItem sapaItem, ArrayList<Integer> laneTypes) {
        if (sapaItem == null) {
            Logger.i(TAG, "updateTollDetailInfo sapItem is null");
            return;
        }
        powerType = OpenApiHelper.powerType();
        Logger.i(TAG, "powerType = " + powerType);
        if (powerType == 1) {
            GeoPoint point = sapaItem.getPos();
            OpenApiHelper.getTravelTimeFutureIncludeChargeLeft(point).thenAccept(etaInfo -> {
                ThreadManager.getInstance().postUi(() -> {
                    if (etaInfo == null) {
                        return;
                    }
                    int leftCharge = etaInfo.getLeftCharge();
                    if (mScreenView != null) {
                        mScreenView.updateTollChargeUi(leftCharge);
                    }
                });
            }).exceptionally(error -> {
                Logger.e(TAG, "updateTollDetailInfo getTravelTimeFuture error:", error);
                return null;
            });
        } else {
            if (mScreenView != null) {
                mScreenView.hideChargeUi();
            }
        }
        mTollName.set(sapaItem.getName());
        tagUpdate(mTollStatusTag, sapaItem);
        updateDistanceAndRemainTime(sapaItem, mTollDistance, mTollRemainTime);
        updateTollDetail(laneTypes, mTollEtcVisible, mTollAlipayVisible);
        /*String areaName = getAreaName();
        mTollRouteName.set(areaName);*/
    }

    /**
     * @return 返回当前城市的名称
     */
    private String getAreaName(int cityCode) {
        final AdminCodeBean adminCode = new AdminCodeBean();
        if (cityCode == NumberUtils.NUM_ERROR) {
            return "";
        }
        adminCode.setnAdCode(cityCode);
        final AreaExtraInfoBean areaExtraInfo = MapDataPackage.getInstance().getAreaExtraInfo(adminCode);
        if (areaExtraInfo == null) {
            return "";
        }
        String areaName = "";
        String provName = areaExtraInfo.getProvName();
        String cityName = areaExtraInfo.getCityName();
        String townName = areaExtraInfo.getTownName();
        if (null != provName && provName.equals(cityName)) {
            areaName = cityName + townName;
        } else {
            areaName = provName + cityName + townName;
        }
        return areaName;
    }

    /**
     * 更新标签
     *
     * @param tag     tag
     * @param sapItem sapItem
     */
    private void tagUpdate(final ObservableField<String> tag,
                           final SapaInfoEntity.SAPAItem sapItem) {
        // 角标显示 途经点>维护/关闭类>其他
        // 判断是否是途经点
        final List<RouteParam> allPoiParamList = RoutePackage.getInstance().
                getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            for (RouteParam routeParam : allPoiParamList) {
                if (Objects.equals(routeParam.getPoiID(), sapItem.getServicePOIID())) {
                    tag.set(mScreenView.getContext().getString(R.string.navi_via));
                    mIsVia = true;
                    mButtonText.set(ResourceUtils.Companion.getInstance().getString(R.string.
                            route_service_details_remove_via));
                    return;
                }
                boolean isCanJudgeGps = null != routeParam.getRealPos() && null != sapItem.getPos();
                if (isCanJudgeGps) {
                    if (routeParam.getRealPos().getLat() == sapItem.getPos().getLat() &&
                            routeParam.getRealPos().getLon() == sapItem.getPos().getLon()) {
                        tag.set(mScreenView.getContext().getString(R.string.navi_via));
                        mIsVia = true;
                        mButtonText.set(ResourceUtils.Companion.getInstance().getString(R.string.
                                route_service_details_remove_via));
                        return;
                    }
                }
            }
        }
        mIsVia = false;
        mButtonText.set(ResourceUtils.Companion.getInstance().getString(R.string.
                route_service_details_add_via));
        //服务区状态:0 非建设中（默认值），1 建设中，2 未调查 3 装修中 4 暂停营业
        final int buildingStatus = sapItem.getBuildingStatus();
        Logger.i(TAG, "tagUpdate buildingStatus = ", buildingStatus);
        switch (buildingStatus) {
            case 0:
                tag.set(SceneNaviSapaImpl.UN_BUILDING);
                break;
            case 1:
                tag.set(SceneNaviSapaImpl.BUILDING);
                break;
            case 3:
                tag.set(SceneNaviSapaImpl.REMODELING);
                break;
            case 4:
                tag.set(SceneNaviSapaImpl.TEMPORARILY_CLOSED);
                break;
            default:
                tag.set(SceneNaviSapaImpl.UN_INVESTIGATION);
                break;
        }
    }

    /**
     * 更新剩余距离和剩余时间信息
     *
     * @param sapItem        sapaItem
     * @param remainDistance remainDistance
     * @param remainTime     remainTime
     */
    private void updateDistanceAndRemainTime(final SapaInfoEntity.SAPAItem sapItem,
                                             final ObservableField<String> remainDistance,
                                             final ObservableField<String> remainTime) {
        final int distance = sapItem.getRemainDist();
        final long time = sapItem.getRemainTime();
        remainDistance.set(DataHelper.convertMetersToKilometers(distance));
        remainTime.set(DataHelper.convertSecondsToDetailedMinutes(time));
    }

    /**
     * 更新可收费类型详情
     *
     * @param laneTypes laneTypes
     * @param etc       etc
     * @param alipay    alipay
     */
    private void updateTollDetail(final ArrayList<Integer> laneTypes,
                                  final ObservableField<Boolean> etc,
                                  final ObservableField<Boolean> alipay) {
        mTollServicesVisible.set(false);
        etc.set(false);
        alipay.set(false);
        if (laneTypes == null) {
            return;
        }
        for (Integer integer : laneTypes) {
            Logger.i(TAG, "updateTollDetail laneTypes = ", integer);
            // TollLaneTypeETC等于2可以显示etc标识
            if (integer == 2) {
                etc.set(true);
                if (Boolean.FALSE.equals(mTollServicesVisible.get())) {
                    mTollServicesVisible.set(true);
                }
            }
            // TollLaneTypeAliPay等于8可以显示支付宝标识
            if (integer == 8) {
                alipay.set(true);
                if (Boolean.FALSE.equals(mTollServicesVisible.get())) {
                    mTollServicesVisible.set(true);
                }
            }
        }
    }

    /**
     * 更新服务区详情
     *
     * @param sapItem       sapItem
     * @param chargeStation chargeStation
     * @param canteen       canteen
     * @param lavatory      lavatory
     * @param maintenance   maintenance
     * @param buy           buy
     * @param hotel         hotel
     */
    private void updateServiceDetails(final SapaInfoEntity.SAPAItem sapItem,
                                      final ObservableField<Boolean> chargeStation,
                                      final ObservableField<Boolean> canteen,
                                      final ObservableField<Boolean> lavatory,
                                      final ObservableField<Boolean> maintenance,
                                      final ObservableField<Boolean> buy,
                                      final ObservableField<Boolean> hotel) {
        mServicesVisible.set(false);
        // 服务区详情
        final long sapaDetail = sapItem.getSapaDetail();
        for (int i = 0; i < 7; i++) {
            final int nthBit = DataHelper.getNthBit(sapaDetail, i);
            if (i >= 1 && nthBit == 1) {
                if (Boolean.FALSE.equals(mServicesVisible.get())) {
                    mServicesVisible.set(true);
                }
            }
            switch (i) {
                case 6:
                    // 只有电车和插混才显示充电桩
                    int powerType = OpenApiHelper.powerType();
                    if (powerType == 1 || powerType == 2) {
                        chargeStation.set(nthBit == 1);
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
                default:
                    break;
            }
        }
    }

    /**
     * 关闭场景
     */
    public void closeScene() {
        updateSceneVisible(false, true);
    }

    /**
     * @param taskId             taskId,请求的唯一标识
     * @param errorCode          错误码，表示搜索操作的结果状态
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message,
                                     SearchResultEntity searchResultEntity) {
        // 如果当前的搜索id与回调的taskId相同，表示是当前的搜索结果
        if (mSapaSearchId == taskId) {
            mRefreshInfoRunnable = new Runnable() {
                @Override
                public void run() {
                    try {
                        refreshGasStationInfo(searchResultEntity);
                    } catch (Exception e) {
                        Logger.e(TAG, "refreshGasStationInfo error:", e.getMessage());
                    }
                }
            };
            ThreadManager.getInstance().postUi(mRefreshInfoRunnable);
            refreshViaData(searchResultEntity);
        } else if (mSapaGeoSearchId == taskId) {
            // 如果是地理位置搜索结果
            mRefreshInfoRunnable = new Runnable() {
                @Override
                public void run() {
                    try {
                        refreshGeoStationInfo(searchResultEntity);
                    } catch (Exception e) {
                        Logger.e(TAG, "refreshGasStationInfo error:", e.getMessage());
                    }
                }
            };
            ThreadManager.getInstance().postUi(mRefreshInfoRunnable);
            refreshViaData(searchResultEntity);
        } else if (mTollGeoSearchId == taskId) {
            // 如果是地理位置搜索结果
            mRefreshGeoRunnable = new Runnable() {
                @Override
                public void run() {
                    try {
                        refreshTollInfo(searchResultEntity, true);
                    } catch (Exception e) {
                        Logger.e(TAG, "refreshTollInfo error:", e.getMessage());
                    }
                }
            };
            ThreadManager.getInstance().postUi(mRefreshGeoRunnable);
        }
    }

    /**
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    private void refreshViaData(final SearchResultEntity searchResultEntity) {
        if (ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
            Logger.i(TAG, "registerViaData searchResultEntity.getPoiList() is null");
            return;
        }
        mCurrentPoiInfoEntity = searchResultEntity.getPoiList().get(0);
        mCurrentPoiType = searchResultEntity.getPoiType();
    }

    /**
     * 触发添加途经点
     */
    public void onAddRemoveViaClick() {
        if (mCurrentPoiInfoEntity == null || mCurrentPoiType == -1) {
            Logger.i(TAG, "onAddRemoveViaClick mCurrentPoiInfoEntity or mCurrentPoiType is null");
            return;
        }
        Logger.i(TAG, "onAddRemoveViaClick mIsVia = ", mIsVia);
        if (!mIsVia) {
            RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                    mCurrentPoiInfoEntity);
        } else {
            RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                    mCurrentPoiInfoEntity, true);
        }
        updateSceneVisible(false, true);
    }


    // 防止点击穿透
    private final Action mRootClick = () -> {
    };

    public Action getRootClick() {
        return mRootClick;
    }
}
