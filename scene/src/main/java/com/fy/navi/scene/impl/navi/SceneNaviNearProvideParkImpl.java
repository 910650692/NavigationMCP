package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.ui.navi.SceneNaviNearProvidePark;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description:
 */
public class SceneNaviNearProvideParkImpl extends BaseSceneModel<SceneNaviNearProvidePark> implements SearchResultCallback, ILayerPackageCallBack {

    private static final String TAG = "SceneNaviNearProvideParkImpl";
    private ArrayList<NaviParkingEntity> mParkingList = new ArrayList<>();
    private boolean mIsEndParking = false;//终点是否是停车场
    private final SearchPackage mSearchPackage;
    private final RoutePackage mRoutePackage;
    private RouteParam mRouteParam;
    private int mParkingAroundSearchTaskId = -1;
    private int mParkingDestationSearchTaskId = -1;
    private boolean mIsNeedShow = false;
    // 是否已经提示过用户
    private boolean mHadShow = false;
    // 数据是否正在请求
    private boolean mIsOnRequesting = false;
    private static final String KEY = "SceneNaviNearProvideParkImpl_KEY";

    public SceneNaviNearProvideParkImpl(SceneNaviNearProvidePark screenView) {
        super(screenView);
        mRoutePackage = RoutePackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mSearchPackage.registerCallBack(KEY, this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mSearchPackage.unRegisterCallBack(KEY);
    }

    /**
     * 校验停车场信息
     *
     * @param naviEtaInfo
     **/
    public void checkParking(final NaviEtaInfo naviEtaInfo) {
        if (ConvertUtils.isEmpty(naviEtaInfo) || mHadShow) {
            Logger.d(TAG, "checkParking:" + mHadShow);
            return;
        }
        if (isEligible(naviEtaInfo.getAllDist(), 3000)) {
            // 3公里准备数据
            requestParkList();
        } else if (isEligible(naviEtaInfo.getAllDist(), 2000)) {
            mIsNeedShow = true;
            //两公里时展示数据
            if (ConvertUtils.isEmpty(mParkingList)) {
                requestParkList();
            } else {
                updateUi();
            }
        }
    }

    private void updateUi() {
        Logger.i(TAG, "updateUi-size:" + (ConvertUtils.isNull(mParkingList) ? 0 : mParkingList.size()));
        if (!ConvertUtils.isNull(mScreenView) && mIsNeedShow) {
            mScreenView.updateUi(mParkingList);
            mHadShow = true;
        }
    }

    /**
     * @param distance          distance
     * @param distanceCondition distanceCondition
     * @return boolean
     */
    private boolean isEligible(final int distance, final int distanceCondition) {
        return distance > (distanceCondition - 500) && distance <= distanceCondition;
    }

    /**
     * requestParking
     */
    private void requestParkList() {
        Logger.d(TAG, "requestParkList:" + mIsOnRequesting, "dataSize:" + (ConvertUtils.isEmpty(mParkingList) ? 0 : mParkingList.size()));
        if (mIsOnRequesting || !ConvertUtils.isEmpty(mParkingList)) {
            return;
        }
        mIsEndParking = false;
        mParkingList.clear();
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
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
                mIsOnRequesting = true;
                Logger.i(TAG, "开始周边搜索 taskId = " + mParkingAroundSearchTaskId);
            } else {
                Logger.e(TAG, "mRouteParam is null：");
            }
        } else {
            Logger.e(TAG, "allPoiParamList is null：");
        }
        // TODO 这里还需要优化，需要搜索改一下接口
        if (!ConvertUtils.isNull(mParkingList)) {
            ThreadManager.getInstance().postDelay(() -> OpenApiHelper.clearSearchLabelMark(), 2000);
        }
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        mIsOnRequesting = false;
        Logger.d(TAG, "NaviGuidanceModel => errorCode: " + errorCode + ", message: " +
                message + "获得搜索结果 taskId = " + taskId + " SearchResultEntity = " +
                (searchResultEntity == null ? "null" : searchResultEntity.toString()));
        // 因为搜索出结果后需地图上有搜索的结果扎标，但是推荐的停车场只有三个要扎标，所以这里先清楚搜索的扎标，筛选出来要的三个后再做扎标操作
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
            if (!ConvertUtils.isEmpty(mParkingList)) {
                sortParkingList();
                updateUi();
            }
        }
    }

    public void startNaviRightNow(final PoiInfoEntity poiInfoEntity) {
        if (!ConvertUtils.isNull(mScreenView) && !ConvertUtils.isNull(mCallBack)) {
            mScreenView.notifySceneStateChange(false);
            mCallBack.startNaviRightNow(poiInfoEntity);
        } else {
            Logger.e(TAG, "startNaviRightNow failed, callback or screeView is null!");
        }
    }

    /**
     * 按照升序排列
     */
    private void sortParkingList() {
        mParkingList.sort(Comparator.comparingDouble(NaviParkingEntity::getSortDis));
        if (mIsEndParking) {
            // 如果停车场是终点并且当前车位紧张,如果其他有停车场且不是紧张的,则将其放在最前面，终点停车场放第二个
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
        // 只取前三个推荐
        if (mParkingList.size() > 3) {
            mParkingList = new ArrayList<>(mParkingList.subList(0, 3));
        }
        mParkingList.get(0).setRecommend(true);
    }
}
