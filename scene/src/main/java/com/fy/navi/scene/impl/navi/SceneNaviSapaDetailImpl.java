package com.fy.navi.scene.impl.navi;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.ui.navi.SceneNaviSapaDetailView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SceneNaviSapaDetailImpl extends BaseSceneModel<SceneNaviSapaDetailView> implements SearchResultCallback {
    public static final String TAG = SceneNaviSapaDetailImpl.class.getSimpleName();

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
    // 将服务区的poiId存在本地
    private String mServicePoiId = null;

    private int mGasStationSearchId = -1;
    private int mSapaSearchId = -1;

    // 当前的poi信息 （用于添加途经点）
    private PoiInfoEntity mCurrentPoiInfoEntity;
    // 当前的poi是否是途经点
    private boolean mIsVia;
    // 当前的poi类型（用于添加途经点）
    private int mCurrentPoiType = -1;
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
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        SearchPackage.getInstance().registerCallBack("SceneNaviSapaDetailImpl",
                this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        SearchPackage.getInstance().unRegisterCallBack("SceneNaviSapaDetailImpl");
    }

    /**
     * @param isVisible 是否显示
     */
    private void updateSceneVisible(final boolean isVisible){
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviSapaDetailImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange(
                (isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SAPA_DETAIL_INFO);
    }

    /**
     * 处理详情页显示
     * @param type  类型
     * @param sapaInfoEntity 实体类
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        Logger.i(TAG, "skipNaviSapaDetailScene" + ", type = " + type +
                ", sapaInfoEntity = " + sapaInfoEntity);
        if (SERVICE_DETAIL_PAGE == type) {
            doServiceSearch(sapaInfoEntity);
            mViewType.set(0);
            updateServiceDetailInfo(sapaInfoEntity);
            updateSceneVisible(true);
        } else {
            doTollSearch(sapaInfoEntity);
            mViewType.set(1);
            updateTollDetailInfo(sapaInfoEntity);
            updateSceneVisible(true);
        }
    }

    /**
     * 收费站透传动作，因为SapaInfoEntity中不会透出收费站的poiId，所以这边只能进行位置搜索
     * @param sapaInfoEntity 实体类
     */
    private void doTollSearch(final SapaInfoEntity sapaInfoEntity) {
        final SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(0);
        mSapaSearchId = SearchPackage.getInstance().geoSearch(sapaItem.getPos());
        Logger.i(TAG, "doTollSearch mSapaSearchId = " + mSapaSearchId);
    }

    /**
     * 服务区搜索动作
     * @param sapaInfoEntity 实体类
     */
    private void doServiceSearch(final SapaInfoEntity sapaInfoEntity) {
        final SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(0);
        if (sapaItem != null) {
            final long detail = sapaItem.getSapaDetail();
            final String poiId = sapaItem.getServicePOIID();
            // 代表有加油站 进行沿途搜索
            if (DataHelper.getNthBit(detail, 0) == 1) {
                final List<String> poiIds = new ArrayList<>();
                poiIds.add(poiId);
                // 搜索服务区加油站详情
                mGasStationSearchId = SearchPackage.getInstance().doLineDeepInfoSearch(
                        ResourceUtils.Companion.getInstance().
                                getString(R.string.navi_via), poiIds);
                Logger.i(TAG, "加油站详情搜索 taskId = " + mGasStationSearchId);
            }
            // 进行PoiId搜索 为了途经点添加功能
            mSapaSearchId = SearchPackage.getInstance().poiIdSearch(poiId);
            Logger.i(TAG, "doServiceSearch  mSapaSearchId = " + mSapaSearchId);
        }
    }

    /**
     * 刷新服务区加油站信息
     * @param result poi点详情信息
     */
    private void refreshGasStationInfo(final SearchResultEntity result) {
        mServiceGasStationVisible.set(false);
        mServiceGasType92Visible.set(false);
        mServiceGasType95Visible.set(false);
        mServiceGasType98Visible.set(false);
        mServiceGasType0Visible.set(false);
        final List<PoiInfoEntity> poiList = result.getPoiList();
        if (mServicePoiId == null) {
            Logger.i(TAG, "refreshGasStationInfo servicePoiId is null");
            return;
        }
        PoiInfoEntity poiInfo = null;
        if (!ConvertUtils.isEmpty(poiList)) {
            for (PoiInfoEntity poiInfoEntity : poiList) {
                if (mServicePoiId.equals(poiInfoEntity.getPid())) {
                    poiInfo = poiInfoEntity;
                    break;
                }
            }
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


    /**
     * @param sapaInfoEntity 更新服务区详情页的信息
     */
    private void updateServiceDetailInfo(final SapaInfoEntity sapaInfoEntity) {
        final SapaInfoEntity.SAPAItem sapItem = sapaInfoEntity.getList().get(0);
        if (sapItem == null) {
            Logger.i(TAG, "updateServiceDetailInfo sapItem is null");
            return;
        }
        mServicePoiId = sapItem.getServicePOIID();
        mServiceName.set(sapItem.getName());
        tagUpdate(mServiceStatusTag, sapItem);
        updateDistanceAndRemainTime(sapItem, mServiceDistance, mServiceRemainTime);
        updateServiceDetails(sapItem, mServiceChargeStationVisible, mServiceCanteenVisible,
                mServiceLavatoryVisible, mServiceMaintenanceVisible, mServiceBuyVisible,
                mServiceHotelVisible);
    }

    /**
     * @param sapaInfoEntity 更新收费站详情页的信息
     */
    private void updateTollDetailInfo(final SapaInfoEntity sapaInfoEntity) {
        final SapaInfoEntity.SAPAItem sapItem = sapaInfoEntity.getList().get(0);
        if (sapItem == null) {
            Logger.i(TAG, "updateTollDetailInfo sapItem is null");
            return;
        }
        mTollName.set(sapItem.getName());
        tagUpdate(mTollStatusTag, sapItem);
        updateDistanceAndRemainTime(sapItem, mTollDistance, mTollRemainTime);
        updateTollDetail(sapaInfoEntity.getLaneTypes(), mTollEtcVisible, mTollAlipayVisible);
    }

    /**
     * 更新标签
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
            }
        }
        mIsVia = false;
        mButtonText.set(ResourceUtils.Companion.getInstance().getString(R.string.
                route_service_details_add_via));
        //服务区状态:0 非建设中（默认值），1 建设中，2 未调查 3 装修中 4 暂停营业
        final int buildingStatus = sapItem.getBuildingStatus();
        Logger.i(TAG, "tagUpdate buildingStatus = " + buildingStatus);
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
     * @param laneTypes laneTypes
     * @param etc etc
     * @param alipay alipay
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
     * @param sapItem       sapItem
     * @param chargeStation chargeStation
     * @param canteen       canteen
     * @param lavatory       lavatory
     * @param maintenance  maintenance
     * @param buy            buy
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
        updateSceneVisible(false);
    }

    /**
     * @param taskId             taskId,请求的唯一标识
     * @param errorCode          错误码，表示搜索操作的结果状态
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message,
                               final SearchResultEntity searchResultEntity) {
        Logger.i(TAG, "onSearchResult taskId = " + taskId +
                ", errorCode = " + errorCode + ", message = " + message);
        if (searchResultEntity == null) {
            Logger.i(TAG, "onSearchResult searchResultEntity is null");
            return;
        }
        Logger.i(TAG, "onSearchResult searchResultEntity = " +
                searchResultEntity.toString());
        // 如果当前的搜索id与回调的taskId相同，表示是当前的搜索结果
        if (mGasStationSearchId == taskId) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    try {
                        refreshGasStationInfo(searchResultEntity);
                    } catch (Exception e) {
                        Logger.e(TAG, "onSearchResult Exception occurred",
                                e.getMessage());
                    }
                }
            });
        }
        if (mSapaSearchId == taskId) {
            refreshViaData(searchResultEntity);
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
        Logger.i(TAG, "refreshViaData mCurrentPoiInfoEntity = " +
                mCurrentPoiInfoEntity.toString() + ", mCurrentPoiType = " + mCurrentPoiType);
    }

    /**
     * 触发添加途经点
     */
    public void onAddRemoveViaClick() {
        if (mCurrentPoiInfoEntity == null || mCurrentPoiType == -1) {
            Logger.i(TAG, "onAddRemoveViaClick mCurrentPoiInfoEntity or mCurrentPoiType is null");
            return;
        }
        Logger.i(TAG, "onAddRemoveViaClick mIsVia = " + mIsVia);
        if (!mIsVia) {
            RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP,
                    mCurrentPoiInfoEntity);
        } else {
            RoutePackage.getInstance().removeVia(MapType.MAIN_SCREEN_MAIN_MAP,
                    mCurrentPoiInfoEntity, false);
        }
        updateSceneVisible(false);
    }
}
