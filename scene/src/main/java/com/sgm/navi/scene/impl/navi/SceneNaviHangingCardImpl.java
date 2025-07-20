package com.sgm.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.callback.OnPowerChangeListener;
import com.sgm.navi.scene.ui.navi.hangingcard.CardManager;
import com.sgm.navi.scene.ui.navi.hangingcard.CardStatus;
import com.sgm.navi.scene.ui.navi.hangingcard.NaviSceneHangingCard;
import com.sgm.navi.scene.ui.navi.hangingcard.OnCardChangeListener;
import com.sgm.navi.scene.util.HandCardType;
import com.sgm.navi.scene.util.PowerMonitorService;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/14
 * Description: [在这里描述文件功能]
 */
public class SceneNaviHangingCardImpl extends BaseSceneModel<NaviSceneHangingCard> implements OnPowerChangeListener, IGuidanceObserver, SearchResultCallback, OnCardChangeListener {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_HANDING_CARD_IMPL;
    private final String KEY_NAME = TAG + "_KEY";
    public final String KEY_WORD_CHARGE_STATION = "充电桩";
    public final String KEY_WORD_GAS_STATION = "加油站";
    private final int DEFAULT_PAGE = 1;
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private int mChargeSearchId;
    private int mGasSearchId;
    // 停车场搜索请求ID, mParkEndSearchId仅当终点是停车场的时候会用到,有一个先后顺序：先请求一次查看终点是否是停车场，然后根据结果再周边搜索
    private int mParkSearchId, mParkEndSearchId;
    private ArrayList<PoiInfoEntity> mChargeList;
    private ArrayList<PoiInfoEntity> mGasList;
    private ArrayList<PoiInfoEntity> mParkList;
    private PoiInfoEntity mEndPoiInfoEntity;// 终点信息
    private RouteParam mEndRouteParam; // 终点
    private NaviEtaInfo mNaviEtaInfo;
    private NaviPackage mNaviPackage;
    private PowerMonitorService mMonitorService;
    private boolean chargeTipFinished = false;
    private boolean gasTipFinished = false;
    private boolean parkTipFinished = false;
    private boolean isGetParkEndRequesting = false;
    private boolean isGetParkListRequesting = false;

    protected CopyOnWriteArrayList<HandCardType> uiList = new CopyOnWriteArrayList<>();
    private CardStatus mChargeStatus = CardStatus.IDLE, mGasStatus = CardStatus.IDLE, mParkStatus = CardStatus.IDLE;

    public SceneNaviHangingCardImpl(NaviSceneHangingCard screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mChargeList = new ArrayList<>();
        mGasList = new ArrayList<>();
        mParkList = new ArrayList<>();
        mMonitorService = new PowerMonitorService();
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        mNaviPackage.registerObserver(KEY_NAME, this);
        mSearchPackage.registerCallBack(KEY_NAME, this);
        mMonitorService.registerListener(this);
        mMonitorService.startSchedule();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mNaviPackage.unregisterObserver(KEY_NAME);
        mSearchPackage.unRegisterCallBack(KEY_NAME);
        mMonitorService.unRegisterListener(this);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
    }

    /***
     * 因为是静默搜索，所以在这里处理结果
     * @param taskId             taskId,请求的唯一标识
     * @param errorCode          错误码，表示搜索操作的结果状态
     * @param message            错误消息，描述搜索操作的结果信息
     * @param searchResultEntity 搜索结果 {@link SearchResultEntity}，包含具体的搜索结果数据
     */
    @Override
    public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        SearchResultCallback.super.onSilentSearchResult(taskId, errorCode, message, searchResultEntity);
        Logger.d(TAG, "onSilentSearchResult:", taskId);
        if (taskId == mChargeSearchId) {
            assembleChargeList(searchResultEntity);
        } else if (taskId == mGasSearchId) {
            assembleGasList(searchResultEntity);
        } else if (taskId == mParkSearchId) {
            assembleParkList(searchResultEntity);
        } else if (taskId == mParkEndSearchId) {
            assembleParkEnd(searchResultEntity);
        } else {
            // 不是自己的，无需处理
        }
    }

    private void assembleParkEnd(SearchResultEntity searchResultEntity) {
        Logger.d(TAG, "assembleParkEnd:", isGetParkEndRequesting);
        isGetParkEndRequesting = false;
        if (!ConvertUtils.isNull(searchResultEntity) && !ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
            mEndPoiInfoEntity = searchResultEntity.getPoiList().get(0);
            Logger.i(TAG, "assembleParkEnd success!");
        } else {
            Logger.i(TAG, "assembleParkEnd failed!");
        }
    }


    private void assembleParkList(SearchResultEntity searchResultEntity) {
        mParkList.clear();
        //-终点是停车场但停车位紧张导致的推荐，【当前终点】放在列表第二位。
        mParkList.addAll(CardManager.getInstance().getParkList(searchResultEntity, mEndPoiInfoEntity));
        showParkList();
    }

    private void assembleGasList(SearchResultEntity searchResultEntity) {
        isGetParkListRequesting = false;
        mGasList.clear();
        mGasList.addAll(CardManager.getInstance().getStationList(searchResultEntity));
        showGasList();
    }

    private void assembleChargeList(SearchResultEntity searchResultEntity) {
        mChargeList.clear();
        mChargeList.addAll(CardManager.getInstance().getStationList(searchResultEntity));
        showChargeList();
    }

    /***
     * 获取充电站列表
     */
    private void getChargeStationList() {
        mChargeSearchId = mSearchPackage.enRouteKeywordSearch(KEY_WORD_CHARGE_STATION, true);
        Logger.d(TAG, "mChargeSearchId:", mChargeSearchId);
    }

    /***
     * 获取加油站列表
     */
    private void getGasStationList() {
        mGasSearchId = mSearchPackage.enRouteKeywordSearch(KEY_WORD_GAS_STATION, true);
    }

    /***
     * 获取停车场列表-触发条件
     * a.目的地非家与公司
     * b.非常去目的地
     * c.非加油站，充电站
     * d.目的地为非停车场，或者目的地是停车场但停车位紧张
     */
    private void getParkList() {
        // 是否小于3000m
        if (!CardManager.getInstance().isEligible(mNaviEtaInfo.getRemainDist(), 3000)) {
            Logger.d(TAG, "getParkList failed, 距离超过3000米！");
            return;
        }
        if (ConvertUtils.isNull(mEndPoiInfoEntity)) {
            Logger.d(TAG, "getParkList failed，终点信息不存在！");
            return;
        }
        // a.目的地非家与公司或者常去地址
        if (CardManager.getInstance().judgeDestinationIsOftenGo(mEndPoiInfoEntity)) {
            Logger.d(TAG, "目的地是常去地址，无需显示停车场悬挂卡");
            return;
        }
        // 非加油站，充电站
        if (CardManager.getInstance().judgeDestinationIsGasOrChargeStation(mEndPoiInfoEntity)) {
            Logger.d(TAG, "目的地是加油站或者充电站，无需显示停车场悬挂卡");
            return;
        }
        // 终点停车位是否紧张，只有紧张才会获取并显示
        if (CardManager.getInstance().endIsParking(mEndPoiInfoEntity) && !CardManager.getInstance().parkIsCrowed(mEndPoiInfoEntity)) {
            Logger.d(TAG, "终点是停车场，但是车位充足，无需显示停车场悬挂卡！");
            return;
        }
        if (ConvertUtils.isEmpty(mParkList)) {
            isGetParkListRequesting = true;
            mParkSearchId = mSearchPackage.aroundSearch(DEFAULT_PAGE, AppCache.getInstance().getMContext().getString(R.string.st_quick_search_parking), mEndPoiInfoEntity.getPoint(), "2000", true);
        }
    }

    private void showParkList() {
        Logger.d(TAG, "showParkList", "size:", mParkList.size(), "parkTipFinished:",
                parkTipFinished);
        if (!CardManager.getInstance().isEligible(mNaviEtaInfo.getRemainDist(), 2000)) {
            return;
        }
        updateUi(HandCardType.PARK, mParkList, mParkStatus);
    }

    private void showGasList() {
        Logger.d(TAG, "showGasList", "size:", mGasList.size(), "gasTipFinished:",
                gasTipFinished);
        updateUi(HandCardType.GAS, mGasList, mGasStatus);
    }

    private void showChargeList() {
        Logger.d(TAG, "showChargeList-Success!:", mChargeList.size(), "chargeTipFinished:",
                chargeTipFinished);
        updateUi(HandCardType.CHARGE, mChargeList, mChargeStatus);
    }

    public synchronized void updateUi(HandCardType type, List<PoiInfoEntity> data, CardStatus cardStatus) {
        Logger.d(TAG, "updateUi", "type:", type.name(), "cardStatus:", cardStatus.name());
        if (!ConvertUtils.isEmpty(data) && cardStatus != CardStatus.ON_SHOWING) {
            uiList.add(type);
            switch (type) {
                case CHARGE -> mChargeStatus = CardStatus.ON_SHOWING;
                case GAS -> mGasStatus = CardStatus.ON_SHOWING;
                case PARK -> mParkStatus = CardStatus.ON_SHOWING;
                default -> {
                }
            }
            mScreenView.notifyDataChanged();
            mScreenView.notifySceneStateChange(true, false);
        }
    }

    /***
     * 获取终点信息
     * @param naviETAInfo
     */
    private void getEndInfo(NaviEtaInfo naviETAInfo) {
        if (isGetParkEndRequesting) {
            Logger.d(TAG, "正在请求数据中，无需重复发起！");
            return;
        }
        if (ConvertUtils.isNull(naviETAInfo)) {
            return;
        }
        final RouteParam currentParam = mRoutePackage.getEndPoint(mMapTypeId);
        if (ConvertUtils.isNull(currentParam)) {
            Logger.e(TAG, "终点信息获取失败!");
            return;
        }
        if (ConvertUtils.isNull(mEndPoiInfoEntity)) {
            // 优先PoiID搜
            if (!ConvertUtils.isEmpty(currentParam.getPoiID())) {
                isGetParkEndRequesting = true;
                mParkEndSearchId = mSearchPackage.poiIdSearch(currentParam.getPoiID(), true);
                Logger.d(TAG, "poiIdSearch getPoiID:", currentParam.getPoiID(), " mParkEndSearchId:", mParkEndSearchId);
            } else if (!ConvertUtils.isNull(currentParam.getRealPos())) {
                isGetParkEndRequesting = true;
                mParkEndSearchId = mSearchPackage.geoSearch(currentParam.getRealPos());
                Logger.d(TAG, "geoSearch mParkEndSearchId:", mParkEndSearchId);
            } else {
                Logger.e(TAG, "终点信息获取失败,POID和终点坐标都为空!");
            }
        } else if (ConvertUtils.isEmpty(mParkList)) {
            getParkList();
        } else {
            showParkList();
        }
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        IGuidanceObserver.super.onNaviInfo(naviETAInfo);
        this.mNaviEtaInfo = naviETAInfo;
        getEndInfo(naviETAInfo);
    }

    @Override
    public void onElectricLowerNotify() {
        Logger.d(TAG, "onElectricLowerNotify");
        getChargeStationList();
    }

    @Override
    public void onGasLowerNotify() {
        Logger.d(TAG, "onGasLowerNotify");
        getGasStationList();
    }

    /***
     * 获取对应数据
     * @param cardType
     * @return
     */
    public List<PoiInfoEntity> getData(final HandCardType cardType) {
        return switch (cardType) {
            case CHARGE -> mChargeList;
            case GAS -> mGasList;
            case PARK -> mParkList;
            default -> new ArrayList<>();
        };
    }

    public CopyOnWriteArrayList<HandCardType> getUiList() {
        return uiList;
    }

    @Override
    public void onTimerFinished(HandCardType type) {
        uiList.remove(type);
        mScreenView.notifyDataChanged();
    }

    @Override
    public void onChangeDestinationSuccess(HandCardType type) {
        uiList.clear();
        mScreenView.notifyDataChanged();
    }

    @Override
    public void onShowDetail(HandCardType type) {
        // TODO
    }

    @Override
    public void expandAll() {
        mScreenView.notifyDataChanged();
    }
}
