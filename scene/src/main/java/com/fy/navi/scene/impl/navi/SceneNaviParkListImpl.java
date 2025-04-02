package com.fy.navi.scene.impl.navi;

import static android.view.View.VISIBLE;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.INaviParkItemClickListener;
import com.fy.navi.scene.api.navi.ISceneNaviParkList;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviParkListView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.search.SearchLoadingDialog;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ScheduledFuture;


public class SceneNaviParkListImpl extends BaseSceneModel<SceneNaviParkListView> implements
        ISceneNaviParkList, SearchResultCallback, INaviParkItemClickListener, ILayerPackageCallBack {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    private int mTimes = NumberUtils.NUM_8;

    private ScheduledFuture mScheduledFuture;
    private ISceneCallback mISceneCallback;
    private ArrayList<NaviParkingEntity> mParkingList = new ArrayList<>();
    private boolean mIsEndParking = false;//终点是否是停车场
    private MapPackage mMapPackage;
    private LayerPackage mLayerPackage;
    private SearchPackage mSearchPackage;
    private RoutePackage mRoutePackage;
    private RouteParam mRouteParam;
    private boolean mIsRequesting = false;//是否在请求停车场信息
    private int mParkingAroundSearchTaskId = -1;
    private int mParkingDestationSearchTaskId = -1;
    private SearchLoadingDialog mSearchLoadingDialog;
    public SceneNaviParkListImpl(final SceneNaviParkListView screenView) {
        super(screenView);
        resetState();
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage.registerCallBack("SceneNaviParkListImpl", this);
    }

    @Override
    protected void onCreate() {
        mLayerPackage.registerCallBack(mMapTypeId, this, LayerType.LAYER_SEARCH);
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * @param sceneCallback addSceneCallback
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    protected void onDestroy() {
        mLayerPackage.clearSearchParkPoi(MapType.MAIN_SCREEN_MAIN_MAP);
        mSearchPackage.unRegisterCallBack("SceneNaviParkListImpl");
        mLayerPackage.unRegisterCallBack(mMapTypeId, this, LayerType.LAYER_SEARCH);
        resetState();
        super.onDestroy();
    }

    @Override
    public void closeParkList() {
        updateSceneVisible(false);
        mLayerPackage.clearSearchParkPoi(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 校验停车场信息
     * @param naviEtaInfo
     **/
    public void checkParking(final NaviEtaInfo naviEtaInfo) {
        if (ConvertUtils.isEmpty(naviEtaInfo)) {
            return;
        }
        Logger.i(TAG, "allDist：" +
                naviEtaInfo.getAllDist() + ",mParkingList " + mParkingList.size());
//        if (isEligible(naviEtaInfo.getAllDist(), 3000) &&
//                ConvertUtils.isEmpty(mParkingList)) {//三公里时请求停车场信息
//            requestParking();
//        } else if (isEligible(naviEtaInfo.getAllDist(), 2000) &&
//                !mIsNeedShowParking) {//两公里时展示数据
//            mIsNeedShowParking = true;
//            if (!ConvertUtils.isEmpty(mParkingList)) {
//                showNaviParkList();
//            } else {
//                requestParking();
//            }
//        }
        requestParking();
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message,
                               final SearchResultEntity searchResultEntity) {
        Logger.d(TAG, "NaviGuidanceModel => errorCode: " + errorCode + ", message: " + message);
        Logger.i(TAG, "获得搜索结果 taskId = " + taskId);
        if (mParkingDestationSearchTaskId == taskId) {
            Logger.i(TAG, "获得终点搜结果");
            if (ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                Logger.e(TAG, "NaviGuidanceModel searchResultEntity.getPoiList is null：");
                return;
            }
            final List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
            final PoiInfoEntity poiInfoEntity = poiList.get(0);
            final String poiTag = poiInfoEntity.getPoiTag();
            final String name = poiInfoEntity.getName().trim();
            Logger.i(TAG, "终点停车场信息：" + poiInfoEntity.toString());
            Logger.i(TAG, "NaviGuidanceModel poiTag：" + poiTag + ",name：" + name);
            //是停车场类型，然后根据终点poi执行周边搜
            if ((!ConvertUtils.isEmpty(poiTag) &&
                    poiTag.contains(
                            AppContext.getInstance().getMContext().getString(
                                    com.fy.navi.scene.R.string.st_quick_search_parking)))) {
                mIsEndParking = true;
                mParkingList.add(NaviDataFormatHelper.getNaviParkingEntity(poiInfoEntity, true));
            }
            if (mRouteParam != null) {
                mSearchPackage.aroundSearch(
                        1,
                        AppContext.getInstance().getMContext().getString(
                                com.fy.navi.scene.R.string.st_quick_search_parking),
                        mRouteParam.getRealPos());
            } else {
                Logger.e(TAG, "mRouteParam is null：");
            }
        } else if (mParkingAroundSearchTaskId == taskId) {
            final List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
            Logger.i(TAG, "周边停车场信息：" + poiList.toString());
            Logger.i(TAG, "getPoiList：" + poiList.size() + ",mIsEndParking：" + mIsEndParking);
            if (!ConvertUtils.isEmpty(poiList)) {
                for (int i = 0; i < poiList.size(); i++) {
                    final PoiInfoEntity poiInfoEntity = poiList.get(i);
                    Logger.i(TAG, "NaviGuidanceModel naviListEntity.getName：" + poiInfoEntity.getName());
                    if (mIsEndParking && i == 0) {
                        continue;
                    }
                    mParkingList.add(NaviDataFormatHelper.getNaviParkingEntity(poiInfoEntity, false));
                }
            }
            if (null != mSearchLoadingDialog) {
                mSearchLoadingDialog.hide();
            }
            mIsRequesting = false;
            showNaviParkList();
//            initTimer();
        }
    }

    /**
     * 显示停车点列表
     */
    private void showNaviParkList() {
        if (!ConvertUtils.isEmpty(mParkingList)) {
            sortParkingList();
            mScreenView.showNaviParkList(mParkingList, true, 0);
            updateSceneVisible(true);
        }
    }

    /**
     * @param select select
     */
    public void showParkingMark(final int select) {
        mLayerPackage.updateSearchParkPoi(mMapTypeId, mParkingList);
        final PreviewParams previewParams = new PreviewParams();
        previewParams.setMapBound(getParkingBound(mParkingList));
        mMapPackage.showPreview(mMapTypeId, previewParams);
        mLayerPackage.setParkFocus(mMapTypeId, String.valueOf(select), true);
    }

    /**
     * @param pois pois
     * @return RectDouble
     */
    public static PreviewParams.RectDouble getParkingBound(
            final ArrayList<NaviParkingEntity> pois) {
        // TODO: 2025/2/17 还需当前定位点
//        NeLocation location = LocationManager.getInstance().getLastLocation();
//        BizPointBusinessInfo info = new BizPointBusinessInfo();
//        info.mPos3D.lon = location.lon;
//        info.mPos3D.lat = location.lat;
//        pois.add(info);
        try {
            double x1 = Double.MAX_VALUE;
            double y1 = Double.MAX_VALUE;
            double x2 = Double.MIN_VALUE;
            double y2 = Double.MIN_VALUE;
            for (int i = 0; i < pois.size(); i++) {
                final NaviParkingEntity oItem = pois.get(i);
                x1 = Math.min(x1, oItem.getPoint().getLon());
                y1 = Math.min(y1, oItem.getPoint().getLat());
                x2 = Math.max(x2, oItem.getPoint().getLon());
                y2 = Math.max(y2, oItem.getPoint().getLat());
            }
            return new PreviewParams.RectDouble(x1, x2, y2, y1);
        } catch (Exception e) {
            Logger.e(TAG, "getParkingBound error" + e.getMessage());
            return null;
        }
    }

    /**
     * 按照升序排列
     */
    private void sortParkingList() {
        mParkingList.sort(Comparator.comparingDouble(NaviParkingEntity::getSortDis));
        if (mIsEndParking) {
            // 如果停车场是终点并且当前车位紧张
            if (!ConvertUtils.isEmpty(mParkingList)) {
                final NaviParkingEntity entity = mParkingList.get(0);
                if (AppContext.getInstance().getMContext().
                        getString(com.android.utils.R.string.navi_parking_tight).
                        equals(entity.getTag())) {
                    for (int i = 1; i < mParkingList.size(); i++) {
                        final NaviParkingEntity item = mParkingList.get(i);
                        if (!AppContext.getInstance().getMContext().
                                getString(com.android.utils.R.string.navi_parking_tight).
                                equals(item.getTag())) {
                            mParkingList.remove(i);
                            mParkingList.add(0, item);
                            break;
                        }
                    }
                }
            }
        }
        mParkingList.get(0).setRecommend(true);
    }

    /**
     * @param distance distance
     * @param distanceCondition distanceCondition
     * @return boolean
     */
    private boolean isEligible(final int distance, final int distanceCondition) {
        return distance > (distanceCondition - 500) && distance <= distanceCondition;
    }

    /**
     * resetState
     */
    private void resetState() {
        mIsRequesting = false;
        mParkingList.clear();
    }

    /**
     * requestParking
     */
    private void requestParking() {
        Logger.i(TAG, " isRequesting：" + mIsRequesting);
        if (mIsRequesting) {
            return;
        }
        mIsEndParking = false;
        mParkingList.clear();
        mIsRequesting = true;
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            // 获取终点的路由参数
            mRouteParam = allPoiParamList.get(allPoiParamList.size() - 1);
            if (mRouteParam != null) {
                Logger.i(TAG, "NaviGuidanceModel routeParam PoiID：" +
                        mRouteParam.getPoiID() + ",realPos：" + mRouteParam.getRealPos().toString());
                // 获取终点的poi信息
                PoiInfoEntity poiInfo = mRouteParam.getMPoiInfoEntity();
                String poiTag = poiInfo.getPoiTag();
                Logger.i(TAG, "终点 poiTag = " + poiTag);
                // 判断终点是否是停车场
                mIsEndParking = !ConvertUtils.isEmpty(poiTag) &&
                        poiTag.contains(
                                AppContext.getInstance().getMContext().getString(
                                        com.fy.navi.scene.R.string.st_quick_search_parking));
                // 搜终点的停车场信息（因为需要实时的信息所以每次请求都要搜索）
                if (mIsEndParking) {
                    mParkingDestationSearchTaskId =
                            mSearchPackage.poiIdSearch(mRouteParam.getPoiID());
                    Logger.i(TAG, "开始终点搜 taskId = " + mParkingDestationSearchTaskId);
                }
                // 进行搜索操作(终点周边搜索)
                mParkingAroundSearchTaskId = OpenApiHelper.getParkSearchResult();
                if (mScreenView.getVisibility() != VISIBLE) {
                    if (null == mSearchLoadingDialog) {
                        mSearchLoadingDialog = new SearchLoadingDialog(mScreenView.getContext());
                    }
                    mSearchLoadingDialog.show();
                }
            } else {
                Logger.e(TAG, "mRouteParam is null：");
            }
        } else {
            Logger.e(TAG, "allPoiParamList is null：");
        }
    }

    @Override
    public void onItemClick(final int listSize, final int position,
                            final NaviParkingEntity entity) {
        Logger.i(TAG, "position = " + position + ",entity：" + entity.getName());
        if (listSize - 1 == position && entity.isEndPoi()) {
            mScreenView.showNaviParkList(mParkingList, false, 0);
        } else {
            mLayerPackage.setParkFocus(mMapTypeId, String.valueOf(position), true);
            mMapPackage.setMapCenter(mMapTypeId, entity.getPoint());
            mScreenView.notifyList(mParkingList, position);
        }
    }

    @Override
    public void onNaviClick(final int position, final NaviParkingEntity entity) {
        Logger.i(TAG, "position：" + position + ",entity：" + entity.getName());
        mRoutePackage.requestChangeEnd(mMapTypeId, NaviDataFormatHelper.getPoiInfoEntity(entity));
    }

    @Override
    public void onNotifyClick(final MapType mapTypeId, final GemBaseLayer layer,
                              final GemLayerItem item) {
        if (item == null) {
            Logger.e(TAG, "pItem == null");
            return;
        }
        final int index = (int) item.getIndex();
        Logger.i(TAG, "position：" + index);
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                mScreenView.showNaviParkList(mParkingList, false, index);
            }
        });
    }

    /**
     * @param isVisible isVisible
     */
    private void updateSceneVisible(final boolean isVisible) {
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviParkListImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SCENE_PARK_LIST);
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
                ThreadManager.getInstance().postUi(new Runnable() {
                    @Override
                    public void run() {
                        updateSceneVisible(false);
                    }
                });
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

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
