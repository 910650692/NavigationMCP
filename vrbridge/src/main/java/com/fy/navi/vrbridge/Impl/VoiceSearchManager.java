package com.fy.navi.vrbridge.Impl;

import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.util.Pair;
import android.util.SparseArray;

import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.PoiBean;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.greendao.favorite.Favorite;
import com.fy.navi.service.greendao.favorite.FavoriteManager;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.vrbridge.IVrBridgeConstant;
import com.fy.navi.vrbridge.VoiceConvertUtil;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * 语音搜索处理类，用于多轮交互
 */
public class VoiceSearchManager {

    private String mSessionId; //多轮对话标识
    private ArrivalBean mDestInfo; //搜索/导航意图条件集合
    private PoiCallback mPoiCallback; //给语音的搜索结果回调
    private int mSearchType; //语音内部使用，当前语音搜索/导航指令类型，用于多轮
    private int mSearchTaskId; //搜索指令返回的task值
    private String mKeyword;
    private String mRouteType;
    private List<PoiInfoEntity> mSearchResultList; //多轮选择保存的搜索结果

    private List<String> mPointNameList; //多目的地的名称集合
    private List<PoiInfoEntity> mMultipleDestPoiList; //多目的地每次搜索结果保存

    private String mPoiType; //地址类型，用于设置家、公司、普通点

    //语音结果回调
    private RespCallback mRespCallback;



    public static VoiceSearchManager getInstance() {
        return VoiceSearchManagerHolder.instance;
    }

    private static final class VoiceSearchManagerHolder {
        private static final VoiceSearchManager instance = new VoiceSearchManager();
    }

    private VoiceSearchManager() {
        mSearchTaskId = -1;
        mSearchType = IVrBridgeConstant.VoiceSearchType.DEFAULT;
        mKeyword = null;
        mSearchResultList = new ArrayList<>();
        mPointNameList = new ArrayList<>();
        mMultipleDestPoiList = new ArrayList<>();
        SearchPackage.getInstance().registerCallBack(IVrBridgeConstant.TAG, mSearchCallback);
    }

    //清空保存的语音多轮数据
    private void clearData() {
        mSessionId = null;
        mSearchTaskId = -1;
        mSearchType = -1;
        mRouteType = null;
        mSearchResultList.clear();
        mPointNameList.clear();
        mMultipleDestPoiList.clear();
    }

    private final SearchResultCallback mSearchCallback = new SearchResultCallback() {

        @Override
        public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
            String keyword = "";
            if (null != searchResultEntity) {
                keyword = searchResultEntity.getKeyword();
            }
            if (!(mSearchTaskId == taskId || Objects.equals(mKeyword, keyword))) {
                return;
            }

            boolean searchSuccess = true;
            if (null == searchResultEntity || null == searchResultEntity.getPoiList() || searchResultEntity.getPoiList().isEmpty()) {
                Log.e(IVrBridgeConstant.TAG, "searchResult is empty");
                searchSuccess = false;
            }

            List<PoiInfoEntity> poiInfoEntityList
                    = searchSuccess ? searchResultEntity.getPoiList() : new ArrayList<>();
            //根据语音搜索类型执行下一步
            switch (mSearchType) {
                case IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD:
                case IVrBridgeConstant.VoiceSearchType.WITH_PREFER:
                    //仅目的地或带偏好
                    dealSingleDestinationResult(searchSuccess, poiInfoEntityList);
                    break;
                case IVrBridgeConstant.VoiceSearchType.ALONG_WAY:
                    //沿途
                    dealAlongWaySearchResult(poiInfoEntityList);
                    break;
                case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                    //设置公司或家地址为当前位置，逆地址搜索回调
                    if (searchSuccess) {
                        PoiInfoEntity poiInfoEntity = poiInfoEntityList.get(0);
                        updateHomeCompany(poiInfoEntity);
                    } else {
                        Log.e(IVrBridgeConstant.TAG, "reverseGeoSearch is empty, can't setHomeCompany");
                        if (null != mPoiCallback) {
                            mPoiCallback.onResponse(CallResponse.createFailResponse("未找到相关结果，试试别的吧"));
                        }
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.SHOW_POI_DETAIL:
                    //查询当前位置
                    if (searchSuccess) {
                        PoiInfoEntity poiInfoEntity = poiInfoEntityList.get(0);
                        showPoiDetail(poiInfoEntity);
                    } else if (null != mRespCallback) {
                        mRespCallback.onResponse(CallResponse.createFailResponse("不好意思，我定位不到你在哪里"));
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                    //收藏当前位置
                    if (searchSuccess) {
                        PoiInfoEntity poiInfoEntity = poiInfoEntityList.get(0);
                        addCommonFavorite(poiInfoEntity);
                    } else if (null != mRespCallback) {
                        mRespCallback.onResponse(CallResponse.createFailResponse("未查询到定位信息，无法收藏"));
                    }
                default:
                    break;
            }
        }

        @Override
        public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
            //静默搜索结果回调，多筛选条件和多目的地搜
            if (mSearchTaskId != taskId) {
                return;
            }

            boolean searchSuccess = true;
            if (null == searchResultEntity || null == searchResultEntity.getPoiList() || searchResultEntity.getPoiList().isEmpty()) {
                Log.e(IVrBridgeConstant.TAG, "searchResult is empty");
                searchSuccess = false;
            }
            PoiInfoEntity firstSearchResult = null;
            if (searchSuccess) {
                firstSearchResult = searchResultEntity.getPoiList().get(0);
                mSearchResultList.clear();
                mSearchResultList.addAll(searchResultEntity.getPoiList());
            }

            switch (mSearchType) {
                case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                    //一个或多个途径点
                    dealMultipleDestResult(searchSuccess, firstSearchResult);
                    break;
                case IVrBridgeConstant.VoiceSearchType.WITH_CONDITION:
                    //先搜索中心点再打开周边搜界面
                    dealConditionCenterResult(searchSuccess, firstSearchResult);
                    break;
                case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                    //设置家和公司
                    dealHomeCompanyResult(searchSuccess, mSearchResultList);
                    break;
                case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                    //搜索Poi信息，添加收藏
                    dealAddFavoriteResult(searchSuccess, mSearchResultList);
                    break;
                case IVrBridgeConstant.VoiceSearchType.TIME_AND_DIST:
                    dealEtaResult(searchSuccess, firstSearchResult);
                    break;
                default:
                    break;
            }
        }
    };

    /**
     * 处理NaviCommandImpl.onRouteNavi.
     *
     * @param sessionId 多轮同步id.
     * @param arrivalBean 搜索/导航目的地条件集合.
     * @param poiCallback 搜索结果回调.
     */
    public void handleCommonSearch(String sessionId, ArrivalBean arrivalBean, PoiCallback poiCallback) {
        if (null == poiCallback) {
            return;
        }
        mPoiCallback = poiCallback;
        if (null == arrivalBean || TextUtils.isEmpty(arrivalBean.getDest())) {
            Log.e(IVrBridgeConstant.TAG, "voiceSearchParams is null");
            mPoiCallback.onResponse(CallResponse.createFailResponse("空目的地"));
            return;
        }

        mSessionId = sessionId;
        mDestInfo = arrivalBean;
        //目的地
        String dest = mDestInfo.getDest();
        //路线偏好
        String routeType = mDestInfo.getRouteType();
        //检索条件
        Map<String, String> conditions = mDestInfo.getConditions();
        //途径点
        SparseArray<PoiBean> passByPoi = mDestInfo.getPassbyPoi();
        //根据输入的目的地封装参数判断当前搜索/导航意图
        if (null == passByPoi || passByPoi.size() == 0) {
            if (null == conditions || conditions.isEmpty()) {
                if (null == routeType) {
                    //没有过滤搜索条件，直接关键字搜索
                    mSearchType = IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD;
                } else {
                    //带有算路偏好的目的地
                    mSearchType = IVrBridgeConstant.VoiceSearchType.WITH_PREFER;
                }
            } else {
                //带有筛选条件，有六项，最多同时存在三条
                mSearchType = IVrBridgeConstant.VoiceSearchType.WITH_CONDITION;
            }
        } else {
            //多目的（一个目的地和一到多个途径点）
            mSearchType = IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY;
        }
        Log.d(IVrBridgeConstant.TAG, " keywordSearch voiceInnerType: " + mSearchType);
        switch (mSearchType) {
            case IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD:
                mRouteType = null;
                disposeSingleDest(dest);
                break;
            case IVrBridgeConstant.VoiceSearchType.WITH_PREFER:
                mRouteType = routeType;
                disposeSingleDest(dest);
                break;
            case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                //多目的地
                mRouteType = null;
                mMultipleDestPoiList.clear();
                mPointNameList.clear();
                mPointNameList.add(dest);
                int size = passByPoi.size();
                for (int i = 0; i < size; i++) {
                    PoiBean poiBean = passByPoi.valueAt(i);
                    if (null == poiBean || TextUtils.isEmpty(poiBean.getName())) {
                        continue;
                    }
                    mPointNameList.add(poiBean.getName());
                }
                disposeMultipleDest();
                break;
            case IVrBridgeConstant.VoiceSearchType.WITH_CONDITION:
                disposeConditionSearch();
                break;
            default:
                Log.w(IVrBridgeConstant.TAG, "unHandle searchType: " + mSearchType);
                break;
        }
    }

    //处理单目的地（带偏好）的搜索/导航意图
    private void disposeSingleDest(String dest) {
        if (null == dest || dest.isEmpty()) {
            return;
        }

        if (IVrBridgeConstant.DestType.HOME.equals(dest)) {
            //回家
           PoiInfoEntity homeInfo = getHomeCompanyPoiInfo(1);
           if (null != homeInfo) {
               planRoute(homeInfo, null);
           }
        } else if (IVrBridgeConstant.DestType.COMPANY.equals(dest)) {
            //去公司
            PoiInfoEntity companyInfo = getHomeCompanyPoiInfo(2);
            if (null != companyInfo) {
                planRoute(companyInfo, null);
            }
        } else {
            jumpToSearchPage(dest);
        }
    }

    //获取保存的家或公司信息，1-家  2-公司
    private PoiInfoEntity getHomeCompanyPoiInfo(int type) {
        List<Favorite> favoriteList = FavoriteManager.getInstance().getValueByCommonName(type);
        if (null == favoriteList || favoriteList.isEmpty()) {
            return null;
        }
        Favorite favoriteItem = favoriteList.get(0);
        return VoiceConvertUtil.getPoiInfoByFavorite(favoriteItem);
    }

    //根据输入的关键字跳转到搜索结果页.
    private void jumpToSearchPage(String keyword) {
        if (null == keyword || keyword.isEmpty()) {
            return;
        }

        mKeyword = keyword;
        Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, keyword);
        MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    //根据搜索结果处理单目的地(或带偏好)的搜索结果
    private void dealSingleDestinationResult(boolean success, List<PoiInfoEntity> searchResultList) {
        if (!success) {
            if (null != mPoiCallback) {
                mPoiCallback.onResponse(CallResponse.createFailResponse("搜索结果为空"));
            }
            return;
        }

        int count = searchResultList.size();
        if (count == 1) {
            //关键字搜结果只有一个，直接选择结果作为目的地发起算路
            PoiInfoEntity endPoi = searchResultList.get(0);
            planRoute(endPoi, null);
        } else {
            //搜索结果为多个，回调给语音，列表多轮选择
            mSearchResultList.clear();
            mSearchResultList.addAll(searchResultList);
            List<PoiBean> poiBeanList = VoiceConvertUtil.convertSearchResult(searchResultList);
            if (null != mPoiCallback) {
                mPoiCallback.onPoiSearch(mSessionId, poiBeanList, poiBeanList.size());
            }
        }
    }

    //根据选中目的地发起算路
    private void planRoute(PoiInfoEntity endPoi, List<PoiInfoEntity> viaList) {
        RouteSpeechRequestParam requestParam = new RouteSpeechRequestParam();
        requestParam.setEndPoiInfoEntity(endPoi);
        requestParam.setMapTypeId(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        if (null != mRouteType) {
            RoutePreferenceID currPrefer = VoiceConvertUtil.convertToAMapPrefer(mRouteType);
            requestParam.setPreferenceID(currPrefer);
            mRouteType = null;
        } else {
            requestParam.setPreferenceID(SettingPackage.getInstance().getRoutePreference());
        }
        if (!(null == viaList || viaList.isEmpty())) {
            requestParam.setViaPoiInfoEntityList(viaList);
        }
        String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
            //当前为导航态，更换目的地直接发起快速导航
            RoutePackage.getInstance().requestRouteFromSpeech(requestParam);
        } else {
            //打开算路界面
            Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ROUTING);
            bundle.putSerializable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST, requestParam);
            MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
        }
    }

    //处理多目的地导航
    private void disposeMultipleDest() {
        if (null == mPointNameList) {
            return;
        }

        String dest = mPointNameList.remove(0);
        if (IVrBridgeConstant.DestType.HOME.equals(dest)) {
            PoiInfoEntity homeInfo = getHomeCompanyPoiInfo(1);
            if (null != homeInfo) {
                dealMultipleDestResult(true, homeInfo);
            }
        } else if (IVrBridgeConstant.DestType.COMPANY.equals(dest)) {
            PoiInfoEntity companyInfo = getHomeCompanyPoiInfo(2);
            if (null != companyInfo) {
                dealMultipleDestResult(true, companyInfo);
            }
        } else {
            mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, dest);
        }
    }

    //根据搜索结果执行多目的地下一步
    private void dealMultipleDestResult(boolean success, PoiInfoEntity poiInfo) {
        if (!success) {
            Log.e(IVrBridgeConstant.TAG, "silentSearch has not result");
            if (null != mPoiCallback) {
                mPoiCallback.onResponse(CallResponse.createFailResponse("搜索结果为空"));
            }
            return;
        }

        mMultipleDestPoiList.add(poiInfo);
        if (mPointNameList.isEmpty()) {
            //途径点和终点都获取到，发起路线规划
            PoiInfoEntity endPoi = mMultipleDestPoiList.remove(0);
            List<PoiInfoEntity> viaList = new ArrayList<>(mMultipleDestPoiList);
            planRoute(endPoi, viaList);
        } else {
            disposeMultipleDest();
        }
    }

    /**
     * Poi多轮根据index选定Poi点.
     *
     * @param sessionId 语音多轮唯一标识.
     * @param index 搜索结果下标.
     */
    public void handlePoiSelectIndex(String sessionId, int index, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "selectPoiIndex: " + index);
        if (null == mSessionId || mSessionId.isEmpty() || !mSessionId.equals(sessionId)) {
            Log.e(IVrBridgeConstant.TAG, "last sessionId is empty or not matching");
            mSearchResultList.clear();
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("sessionId不匹配"));
            }
            return;
        }
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            Log.e(IVrBridgeConstant.TAG, "last searchResult is empty or index overview");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("上一轮搜索结果为空"));
            }
            return;
        }

        if (mSearchResultList.size() > index) {
            PoiInfoEntity poiInfoEntity = mSearchResultList.get(index);
            if (null != poiInfoEntity) {
                disposeSelectedPoi(poiInfoEntity, respCallback);
            }
        } else {
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("超出选择范围"));
            }
        }
    }

    /**
     * 根据条件排序搜索结果列表.
     * @param sessionId 语音多轮唯一标识.
     * @param isDescending true-降序; false-升序
     * @param respCallback 语音传入的结果回调接口
     */
    public void handlePoiSort(String sessionId, String type, boolean isDescending, RespCallback respCallback) {
        if (null == mSessionId || mSessionId.isEmpty() || !mSessionId.equals(sessionId)) {
            mSearchResultList.clear();
            Log.e(IVrBridgeConstant.TAG, "last sessionId is empty or not matching");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("sessionId不匹配"));
            }
            return;
        }

        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            Log.e(IVrBridgeConstant.TAG, "last searchResult is empty");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("上一轮搜索结果为空"));
            }
            return;
        }

        switch (type) {
            case IVrBridgeConstant.PoiSortType.DISTANCE:
                break;
            case IVrBridgeConstant.PoiSortType.PRIZE:
                break;
            case IVrBridgeConstant.PoiSortType.RATE:
                sortRatePoi(isDescending, respCallback);
                break;
            default:
                Log.e(IVrBridgeConstant.TAG, "no match action");
                break;
        }
    }

    private void sortRatePoi(boolean isDescending, RespCallback respCallback) {
        int size = mSearchResultList.size();
//        for (int rearIndex = 1; rearIndex < size; rearIndex++) {
//            if (mSearchResultList.get(rearIndex).getRating() > mSearchResultList.get(rearIndex - 1).getRating())
//            PoiInfoEntity tempPoi = mSearchResultList.get(rearIndex);
//
//        }
    }

    /**
     * 根据条件选择搜索结果列表某一项.
     *
     * @param sessionId 语音多轮唯一标识.
     * @param rule 1-最近的  2-评分最高的  3-价格最低  4-价格最高.
     * @param respCallback 语音传入的结果回调接口.
     */
    public void handlePoiSelectRule(String sessionId, int rule, RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "selectPoi with rule: " + rule);
        if (null == mSessionId || mSessionId.isEmpty() || !mSessionId.equals(sessionId)) {
            mSearchResultList.clear();
            Log.e(IVrBridgeConstant.TAG, "last sessionId is empty or not matching");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("sessionId不匹配"));
            }
            return;
        }
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            Log.e(IVrBridgeConstant.TAG, "last searchResult is empty");
            if (null != respCallback) {
                respCallback.onResponse(CallResponse.createFailResponse("上一轮搜索结果为空"));
            }
            return;
        }

        switch (rule) {
            case 1:
                chooseNearestPoi(respCallback);
                break;
            case 2:
                //HIGHEST
                chooseHighestPoi(respCallback);
                break;
            case 3:
                //CHEAPEST
                chooseCheapestPoi(respCallback);
                break;
            case 4:
                //DEAREST
                chooseDearestPoi(respCallback);
                break;
            default:
                Log.e(IVrBridgeConstant.TAG, "not handle rule: " + rule);
                break;
        }
    }

    //选择搜索结果中距离最近的poi点
    private void chooseNearestPoi(RespCallback respCallback) {
        int size = mSearchResultList.size();
        int distance = 0;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }
            int sortDistance = poiInfoEntity.getSort_distance();
            if (distance <= 0 || distance > sortDistance) {
                distance = sortDistance;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    //选择评分最高的Poi点
    private void chooseHighestPoi(RespCallback respCallback) {
        int size = mSearchResultList.size();
        int rate = -1;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }
            int sortRate = poiInfoEntity.getSort_rate();
            if (rate < 0 || sortRate > rate) {
                rate = sortRate;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    //选择价格最低Poi点
    private void chooseCheapestPoi(RespCallback respCallback) {
        int size = mSearchResultList.size();
        int price = -1;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }

            int sortPrice = poiInfoEntity.getSort_price();
            if (price < 0 || sortPrice < price) {
                price = sortPrice;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    //选择价格最高Poi点
    private void chooseDearestPoi(RespCallback respCallback) {
        int size = mSearchResultList.size();
        int price = -1;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }

            int sortPrice = poiInfoEntity.getSort_price();
            if (price < 0 || sortPrice > price) {
                price = sortPrice;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    //处理最后选中的Poi.
    private void disposeSelectedPoi(PoiInfoEntity poiInfo, RespCallback respCallback) {
        if (null != respCallback) {
            respCallback.onResponse(CallResponse.createSuccessResponse());
        }
        mSearchResultList.clear();
        switch (mSearchType) {
            case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                updateHomeCompany(poiInfo);
                break;
            default:
                planRoute(poiInfo, null);
                break;
        }
    }

    //带条件的目的地检索
    private void disposeConditionSearch() {
        Map<String, String> conditionMap = mDestInfo.getConditions();
        if (null == conditionMap || conditionMap.isEmpty()) {
            return;
        }

        String centerValue = conditionMap.getOrDefault(IVrBridgeConstant.ConditionKey.CENTER, "");
        if (null == centerValue || centerValue.isEmpty()) {
            //没有中心点，当前位置的周边搜
            LocInfoBean curLocation = PositionPackage.getInstance().getLastCarLocation();
            if (null != curLocation) {
                GeoPoint geoPoint = new GeoPoint(curLocation.getLongitude(), curLocation.getLatitude());
                jumpToAroundPage(mDestInfo.getDest(), geoPoint);
            }
        } else {
            //先搜索中心点
            mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, centerValue);
        }
    }

    //处理周边搜中心点结果
    private void dealConditionCenterResult(boolean success, PoiInfoEntity centerInfo) {
        if (!success && null == centerInfo || null == centerInfo.getPoint()) {
            Log.e(IVrBridgeConstant.TAG, "searchAround center is empty");
            if (null != mPoiCallback) {
                mPoiCallback.onResponse(CallResponse.createFailResponse("搜索结果为空"));
            }
            return;
        }

        GeoPoint geoPoint = centerInfo.getPoint();
        jumpToAroundPage(mDestInfo.getDest(), geoPoint);
    }

    /**
     * 根据传入的关键字和经纬度（中心点）跳转到周边搜界面.
     *
     * @param keyword String，搜索关键字.
     * @param geoPoint GeoPoint，周边搜中心点.
     */
    private void jumpToAroundPage(String keyword, GeoPoint geoPoint) {
        if (null == keyword || keyword.isEmpty()) {
            return;
        }

        mKeyword = keyword;
        Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.AROUND_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
        bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.AROUND_POINT, geoPoint);
        MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 处理沿途搜索逻辑.
     *
     * @param sessionId 语音多轮唯一标识.
     * @param passBy 沿途点.
     * @param poiType 沿途点类型.
     * @param poiCallback 执行结果回调.
     */
    public void handlePassBy(String sessionId, String passBy, String poiType, PoiCallback poiCallback) {
        if (TextUtils.isEmpty(sessionId) || TextUtils.isEmpty(passBy)) {
            Log.e(IVrBridgeConstant.TAG, "session or passBy is empty");
            if (null != poiCallback) {
                poiCallback.onResponse(CallResponse.createFailResponse("空的sessionId"));
            }
            return;
        }

        mSessionId = sessionId;
        mPoiCallback = poiCallback;
        //保存沿途搜参数，如果沿途搜没有结果，需转为周边搜
        mDestInfo = new ArrivalBean();
        mDestInfo.setDest(passBy);
        mDestInfo.setDestType(poiType);
        mSearchType = IVrBridgeConstant.VoiceSearchType.ALONG_WAY;
        Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ALONG_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, passBy);
        MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    //处理沿途搜结果，一个或多个结果都需要用户确认添加途径点.
    private void dealAlongWaySearchResult(List<PoiInfoEntity> poiList) {
        if (null == poiList || poiList.isEmpty()) {
            return;
        }

        List<PoiBean> poiBeanList = VoiceConvertUtil.convertSearchResult(poiList);
        int size = poiBeanList.size();
        if (null != mPoiCallback) {
            mPoiCallback.onPoiSearch(mSessionId, poiBeanList, size);
        }
    }

    /**
     * 设置公司/家的地址.
     *
     * @param sessionId String，多轮对话保持一致性
     * @param poiType String，HOME-家，COMPANY-公司
     * @param poi String，CURRENT_LOCATION-当前地址 or poi名称.
     */
    public void setHomeCompany(String sessionId, String poiType, String poi, PoiCallback poiCallback) {
        mSessionId = sessionId;
        mPoiType = poiType;
        mPoiCallback = poiCallback;
        mSearchType = IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY;
        if (IVrBridgeConstant.CURRENT_LOCATION.equals(poi)) {
            //当前地址，逆地理搜索
            LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint);
            }
        } else {
            //搜索Poi点信息
            mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, poi);
        }
    }

    //根据搜索结果设置家/公司地址
    private void dealHomeCompanyResult(boolean searchSuccess, List<PoiInfoEntity> poiList) {
        if (!searchSuccess) {
            Log.w(IVrBridgeConstant.TAG, "setHomeCompany, searchResult is empty");
            if (null != mPoiCallback) {
                mPoiCallback.onResponse(CallResponse.createFailResponse("未找到相关结果，试试别的吧"));
            }
            return;
        }

        int size = poiList.size();
        if (size == 1) {
            updateHomeCompany(poiList.get(0));
        } else {
            if (null != mPoiCallback) {
                List<PoiBean> poiBeanList = VoiceConvertUtil.convertSearchResult(poiList);
                mPoiCallback.onPoiSearch(mSessionId, poiBeanList, size);
            }
        }
    }

    //跟新家-公司地址
    private void updateHomeCompany(PoiInfoEntity poiInfo) {
        if (null == poiInfo || null == poiInfo.getPid()) {
            return;
        }

        FavoriteInfo favoriteInfo = new FavoriteInfo();
        int type;
        if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
            //家
            type = 1;
        } else {
            //公司
            type = 2;
        }
        favoriteInfo.setCommonName(type);
        poiInfo.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavorite(poiInfo);
        BehaviorPackage.getInstance().addFavoriteData(poiInfo, type);
    }

    /**
     * 通过逆地理搜索查询当前位置详细信息.
     *
     * @param searchType，查询当前位置的目的，
     * @param geoPoint 当前定位信息.
     * @param respCallback 语音响应回调.
     */
    public void queryCurrentLocationDetail(int searchType, GeoPoint geoPoint, RespCallback respCallback) {
        if (null != geoPoint) {
            mSearchType = searchType;
            mRespCallback = respCallback;
            mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint);
        }
    }

    //根据逆地理搜索结果展示Poi详情
    private void showPoiDetail(PoiInfoEntity poiInfo) {
        if (null != poiInfo) {
            String address = poiInfo.getAddress();
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createSuccessResponse("当前定位在" + address + "附近"));
            }

            String curState = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NO_STATUS.equals(curState)) {
                //在地图首页，打开Poi详情页面
                Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.POI_DETAIL);
                bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.POI_DETAIL_INFO, poiInfo);
                MapPackage.getInstance().voiceOpenSearchPage(MapTypeId.MAIN_SCREEN_MAIN_MAP, bundle);
            }
        }
    }

    //收藏普通点
    private void addCommonFavorite(PoiInfoEntity poiInfo) {
        if (null != poiInfo) {
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createSuccessResponse("已为你收藏" + poiInfo.getName()));
            }
            FavoriteInfo favoriteInfo = new FavoriteInfo();
            favoriteInfo.setCommonName(0);
            poiInfo.setFavoriteInfo(favoriteInfo);
            BehaviorPackage.getInstance().addFavorite(poiInfo);
            BehaviorPackage.getInstance().addFavoriteData(poiInfo, 0);
        }
    }

    /**
     * 搜索关键字信息.
     *
     * @param searchType 搜索目的.
     * @param keyword 关键字.
     * @param respCallback 语音响应回调.
     */
    public void searchPoiInfo(int searchType, String keyword, RespCallback respCallback) {
        if (TextUtils.isEmpty(keyword)) {
            return;
        }
        mSearchType = searchType;
        mRespCallback = respCallback;
        mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, keyword);
    }

    //根据结果处理收藏流程
    private void dealAddFavoriteResult(boolean success, List<PoiInfoEntity> poiList) {
        if (!success) {
            Log.w(IVrBridgeConstant.TAG, "search " + mKeyword + " result is empty");
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createFailResponse("未找到相关结果，试试别的吧"));
            }
        } else {
            PoiInfoEntity poiInfoEntity = poiList.get(0);
            addCommonFavorite(poiInfoEntity);
        }
    }

    /**
     * 根据搜索获取的结果计算与终点的ETA信息.
     *
     * @param poiType，终点类型 HOME-家  COMPANY-公司.
     * @param keyword 关键字.
     * @param respCallback 语音响应回调.
     */
    public void searchPoiInfo(int searchType, String poiType, String keyword, RespCallback respCallback) {
        if (TextUtils.isEmpty(keyword)) {
            return;
        }
        mSearchType = searchType;
        mPoiType = poiType;
        mKeyword = keyword;
        mRespCallback = respCallback;
        mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, keyword);
    }

    //根据搜索结果，获取到家/公司的TIME_DIST信息
    private void dealEtaResult(boolean success, PoiInfoEntity poiInfo) {
        if (!success || null == poiInfo || null == poiInfo.getPoint()) {
            Log.w(IVrBridgeConstant.TAG, "searchForEta empty result");
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createFailResponse("未找到" + mKeyword + "，试试别的吧"));
            }
            return;
        }

        GeoPoint startPoint = poiInfo.getPoint();
        GeoPoint endPoint = null;
        if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
            //家
            PoiInfoEntity homeInfo = BehaviorPackage.getInstance().getFavoriteHomeData(1);
            if (null != homeInfo && null != homeInfo.getPoint()) {
                endPoint = homeInfo.getPoint();
            }
        } else {
            //公司
            PoiInfoEntity companyInfo = BehaviorPackage.getInstance().getFavoriteHomeData(2);
            if (null != companyInfo && null != companyInfo.getPoint()) {
                endPoint = companyInfo.getPoint();
            }
        }

        if (null != endPoint) {
            RoutePackage.getInstance().getTravelTimeFuture(startPoint, endPoint).thenAccept(new Consumer<Pair<String, String>>() {
                @Override
                public void accept(Pair<String, String> pair) {
                    String distance = pair.first;
                    String time = pair.second;
                    StringBuilder response = new StringBuilder();
                    if (!TextUtils.isEmpty(mKeyword)) {
                        response.append(mKeyword);
                    }
                    if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
                        response.append("到家有");
                    } else {
                        response.append("到公司有");
                    }
                    response.append(distance).append("，大约需要").append(time);
                    String homeCompanyEta = response.toString();
                    Log.d(IVrBridgeConstant.TAG, "homeCompanyEta: " + homeCompanyEta);
                    if (null != mRespCallback) {
                        mRespCallback.onResponse(CallResponse.createSuccessResponse(homeCompanyEta));
                    }
                }
            }).exceptionally(new Function<Throwable, Void>() {
                @Override
                public Void apply(Throwable throwable) {
                    if (null != mRespCallback) {
                        mRespCallback.onResponse(CallResponse.createFailResponse("没有查询到相关信息，试试别的吧"));
                    }
                    return null;
                }
            });
        }
    }

}
