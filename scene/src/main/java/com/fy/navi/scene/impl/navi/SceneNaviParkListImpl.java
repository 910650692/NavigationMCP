package com.fy.navi.scene.impl.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.INaviParkItemClickListener;
import com.fy.navi.scene.api.navi.ISceneNaviParkList;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviParkListView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;


public class SceneNaviParkListImpl extends BaseSceneModel<SceneNaviParkListView> implements ISceneNaviParkList, SearchResultCallback, INaviParkItemClickListener, ILayerPackageCallBack {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ISceneCallback mISceneCallback;
    private ArrayList<NaviParkingEntity> mParkingList = new ArrayList<>();
    private boolean mIsEndParking = false;//终点是否是停车场
    private MapPackage mMapPackage;
    private LayerPackage mLayerPackage;
    private SearchPackage mSearchPackage;
    private RoutePackage mRoutePackage;
    private RouteParam mRouteParam;
    private boolean isRequesting = false;//是否在请求停车场信息
    private boolean isNeedShowParking = false;//是否需要展示停车场信息，只有在两公里处展示
    private boolean isDisplayed = false;

    public SceneNaviParkListImpl(SceneNaviParkListView mScreenView) {
        super(mScreenView);
        resetState();
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage.registerCallBack("SceneNaviParkListImpl", this);
    }

    @Override
    protected void onCreate() {
        mLayerPackage.registerCallBack(mMapTypeId, this, LayerType.SEARCH_LAYER);
    }

    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    protected void onDestroy() {
        mLayerPackage.clearSearchParkPoi(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        mSearchPackage.unRegisterCallBack("SceneNaviParkListImpl");
        mLayerPackage.unRegisterCallBack(mMapTypeId, this, LayerType.SEARCH_LAYER);
        resetState();
        super.onDestroy();
    }

    @Override
    public void closeParkList() {
        updateSceneVisible(false);
        mLayerPackage.clearSearchParkPoi(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    /***校验停车场信息***/
    public void checkParking(NaviEtaInfo naviEtaInfo) {
        if (ConvertUtils.isEmpty(naviEtaInfo)) {
            return;
        }
        Logger.i(TAG, "isDisplayed：" + isDisplayed + ",allDist：" + naviEtaInfo.allDist + ",mParkingList " + mParkingList.size());
        if (isDisplayed) {
            return;
        }
        if (isEligible(naviEtaInfo.allDist, 3000) && ConvertUtils.isEmpty(mParkingList)) {//三公里时请求停车场信息
            requestParking();
        } else if (isEligible(naviEtaInfo.allDist, 2000) && !isNeedShowParking) {//两公里时展示数据
            isNeedShowParking = true;
            if (!ConvertUtils.isEmpty(mParkingList)) {
                showNaviParkList();
            } else {
                requestParking();
            }
        }
//        if (true && !isNeedShowParking) {//测试
//            isNeedShowParking = true;
//            if (!ConvertUtils.isEmpty(mParkingList)) {
//                showNaviParkList();
//            } else {
//                requestParking();
//            }
//        }
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        Logger.d(TAG, "NaviGuidanceModel => errorCode: " + errorCode + ", message: " + message);
        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH) {
            if (ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
                Logger.e(TAG, "NaviGuidanceModel searchResultEntity.getPoiList is null：");
                return;
            }
            List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
            PoiInfoEntity poiInfoEntity = poiList.get(0);
            String poiTag = poiInfoEntity.getPoiTag();
            String name = poiInfoEntity.getName().trim();
            Logger.i(TAG, "NaviGuidanceModel poiTag：" + poiTag + ",name：" + name);
            //是停车场类型，然后根据终点poi执行周边搜
            if ((!ConvertUtils.isEmpty(poiTag) && poiTag.contains(AppContext.mContext.getString(com.fy.navi.scene.R.string.st_quick_search_parking)))) {
                mIsEndParking = true;
                mParkingList.add(NaviDataFormatHelper.getNaviParkingEntity(poiInfoEntity, true));
            }
            if (mRouteParam != null) {
                mSearchPackage.aroundSearch(1, AppContext.mContext.getString(com.fy.navi.scene.R.string.st_quick_search_parking), mRouteParam.getRealPos());
            } else {
                Logger.e(TAG, "mRouteParam is null：");
            }
        } else if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.AROUND_SEARCH) {
            List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
            Logger.i(TAG, "getPoiList：" + poiList.size() + ",mIsEndParking：" + mIsEndParking);
            if (!ConvertUtils.isEmpty(poiList)) {
                for (int i = 0; i < poiList.size(); i++) {
                    if (i == 0 || i == 1 || (i == 2 && !mIsEndParking)) {
                        PoiInfoEntity poiInfoEntity = poiList.get(i);
                        Logger.i(TAG, "NaviGuidanceModel naviListEntity.getName：" + poiInfoEntity.getName());
                        mParkingList.add(NaviDataFormatHelper.getNaviParkingEntity(poiInfoEntity, false));
                    } else {
                        break;
                    }
                }
            }
            isRequesting = false;
            showNaviParkList();
        }
    }

    private void showNaviParkList() {
        Logger.i(TAG, "isNeedShowParking：" + isNeedShowParking + ",mIsEndParking：" + mIsEndParking + ",isDisplayed：" + isDisplayed);
        if (isDisplayed || !isNeedShowParking) {
            return;
        }
        if (!ConvertUtils.isEmpty(mParkingList)) {
            sortParkingList();
            mScreenView.showNaviParkList(mParkingList, true, 0);
            updateSceneVisible(true);
            isDisplayed = true;
        }
    }

    public void showParkingMark(int select) {
        mLayerPackage.updateSearchParkPoi(mMapTypeId, mParkingList);
        PreviewParams previewParams = new PreviewParams();
        previewParams.setMapBound(getParkingBound(mParkingList));
        mMapPackage.showPreview(mMapTypeId, previewParams);
        mLayerPackage.setParkFocus(mMapTypeId, String.valueOf(select), true);
    }

    public static PreviewParams.RectDouble getParkingBound(ArrayList<NaviParkingEntity> pois) {
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
                NaviParkingEntity oItem = pois.get(i);
                x1 = Math.min(x1, oItem.getPoint().lon);
                y1 = Math.min(y1, oItem.getPoint().lat);
                x2 = Math.max(x2, oItem.getPoint().lon);
                y2 = Math.max(y2, oItem.getPoint().lat);
            }
            return new PreviewParams.RectDouble(x1, x2, y2, y1);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * 按照升序排列
     */
    private void sortParkingList() {
        mParkingList.sort(Comparator.comparingDouble(NaviParkingEntity::getSortDis));
        mParkingList.get(0).setRecommend(true);
    }

    private boolean isEligible(int distance, int distanceCondition) {
        return distance > (distanceCondition - 500) && distance <= distanceCondition;
    }

    private void resetState() {
        isNeedShowParking = false;
        isDisplayed = false;
        isRequesting = false;
        mParkingList.clear();
    }

    private void requestParking() {
        Logger.i(TAG, " isRequesting：" + isRequesting);
        if (isRequesting) {
            return;
        }
        mIsEndParking = false;
        mParkingList.clear();
        isRequesting = true;
        List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            mRouteParam = allPoiParamList.get(allPoiParamList.size() - 1);
            if (mRouteParam != null) {
                Logger.i(TAG, "NaviGuidanceModel routeParam PoiID：" + mRouteParam.getPoiID() + ",realPos：" + mRouteParam.getRealPos().toString());
                //若无PoiId，则不能校验终点类型是否是停车场信息，直接发起目的地周边搜停车场信息
                if (!ConvertUtils.isEmpty(mRouteParam.getPoiID())) {
                    mSearchPackage.poiIdSearch(mRouteParam.getPoiID());
                } else {
                    mSearchPackage.geoSearch(mRouteParam.getRealPos());
                }
            } else {
                Logger.e(TAG, "mRouteParam is null：");
            }
        } else {
            Logger.e(TAG, "allPoiParamList is null：");
        }
    }

    @Override
    public void onItemClick(int listSize, int position, NaviParkingEntity entity) {
        Logger.i(TAG, "position：" + position + ",entity：" + entity.getName());
        if (listSize - 1 == position && entity.isEndPoi) {
            mScreenView.showNaviParkList(mParkingList, false, 0);
        } else {
            mLayerPackage.setParkFocus(mMapTypeId, String.valueOf(position), true);
            mMapPackage.setMapCenter(mMapTypeId, entity.getPoint());
            mScreenView.notifyList(mParkingList, position);
        }
    }

    @Override
    public void onNaviClick(int position, NaviParkingEntity entity) {
        Logger.i(TAG, "position：" + position + ",entity：" + entity.getName());
        mRoutePackage.requestRoute(mMapTypeId, NaviDataFormatHelper.getPoiInfoEntity(entity), entity.getPoiType(), true, RouteWayID.ROUTE_WAY_DEFAULT);
    }

    @Override
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        if (pItem == null) {
            Logger.e(TAG, "pItem == null");
            return;
        }
        int index = (int) pItem.getIndex();
        Logger.i(TAG, "position：" + index);
        mScreenView.showNaviParkList(mParkingList, false, index);
    }

    private void updateSceneVisible(boolean isVisible) {
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_PARK_LIST);
    }
}
