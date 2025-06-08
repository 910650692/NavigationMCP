package com.fy.navi.vrbridge.impl;

import android.os.Bundle;
import android.text.TextUtils;
import android.util.SparseArray;

import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.PoiBean;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.baidu.oneos.protocol.result.NaviSubCallResult;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.vrbridge.IVrBridgeConstant;
import com.fy.navi.vrbridge.MapStateManager;
import com.fy.navi.vrbridge.VoiceConvertUtil;
import com.fy.navi.vrbridge.bean.SingleDestInfo;
import com.fy.navi.vrbridge.bean.VoiceSearchConditions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 语音搜索处理类，用于多轮交互
 *
 * @author tssh.
 * @version $Revision.1.0.0$
 */
public final class VoiceSearchManager {

    private String mSessionId; //多轮对话标识
    private ArrivalBean mDestInfo; //搜索/导航意图条件集合
    private PoiCallback mPoiCallback; //给语音的搜索结果回调
    private RespCallback mRespCallback; //语音结果回调
    private int mSearchType; //语音内部使用，当前语音搜索/导航指令类型，用于多轮
    private int mSearchTaskId; //搜索指令返回的task值
    private String mKeyword; //搜索关键字
    private String mRouteType; //路线偏好
    private List<PoiInfoEntity> mSearchResultList; //多轮选择保存的搜索结果
    private boolean mWaitPoiSearch = false; //搜索结果只有一个，需要poi详情搜结果返回后再执行算路

    private List<String> mGenericsList; //DestType为泛型的集合，用来判断途径点和目的地是不是泛型
    private SparseArray<SingleDestInfo> mNormalDestList; //多目的地普通点集合
    private SparseArray<SingleDestInfo> mGenericsDestList; //多目的地泛型点集合
    private int mProcessDestIndex = -1; //当前正在处理的多目的地下标
    private SparseArray<PoiInfoEntity> mMultiplePoiArray; //多目的地途径点和目的地集合

    private String mPoiType; //地址类型，用于设置家、公司、普通点

    private VoiceSearchConditions mSearchCondition; //深度-筛选搜索项
    private String mSortValue; //搜索结果列表排序规则

    private boolean mAlongToAround = false; //是否为沿途搜转周边搜，在沿途搜结果为空时触发
    //起点到终点的ETA信息，保存搜索起终点名称和对应的poi信息
    private List<String> mEtaNameList; //起终点名称，如果只有一个，当前位置为起点
    private List<GeoPoint> mEtaPointList; //起终点经纬度信息

    private boolean mListPageOpened; //搜索结果页面是否展示

    private boolean mShouldPlayRouteMsg; //是否应该播报路线规划结果


    public static VoiceSearchManager getInstance() {
        return VoiceSearchManagerHolder.INSTANCE;
    }

    private static final class VoiceSearchManagerHolder {
        private static final VoiceSearchManager INSTANCE = new VoiceSearchManager();
    }

    private VoiceSearchManager() {
        mSearchTaskId = -1;
        mSearchType = IVrBridgeConstant.VoiceSearchType.DEFAULT;
        mKeyword = null;
        mSearchResultList = new ArrayList<>();

        //当前语义确认的泛型类型为以下
        mGenericsList = new ArrayList<>();
        mGenericsList.add(IVrBridgeConstant.DestType.TOILET);
        mGenericsList.add(IVrBridgeConstant.DestType.ATM);
        mGenericsList.add(IVrBridgeConstant.DestType.BANK);
        mGenericsList.add(IVrBridgeConstant.DestType.SUPERMARKET);
        mGenericsList.add(IVrBridgeConstant.DestType.GAS_STATION);
        mGenericsList.add(IVrBridgeConstant.DestType.CHARGING_STATION);
        mGenericsList.add(IVrBridgeConstant.DestType.RESTAURANT);
        mGenericsList.add(IVrBridgeConstant.DestType.SERVICE_STATION);

        mNormalDestList = new SparseArray<>();
        mGenericsDestList = new SparseArray<>();
        mMultiplePoiArray = new SparseArray<>();

        mEtaNameList = new ArrayList<>();
        mEtaPointList = new ArrayList<>();

        mListPageOpened = SearchPackage.getInstance().isMIsShow();
        SearchPackage.getInstance().registerCallBack(IVrBridgeConstant.TAG, mSearchCallback);
    }

    /**
     * 清空保存的语音多轮数据
     */
    private void clearData() {
        mSessionId = null;
        mSearchTaskId = -1;
        mSearchType = -1;
        mRouteType = null;
        mSearchResultList.clear();
        mWaitPoiSearch = false;
        mAlongToAround = false;

        mNormalDestList.clear();
        mGenericsDestList.clear();
        mMultiplePoiArray.clear();
        mProcessDestIndex = -1;
    }

    private final SearchResultCallback mSearchCallback = new SearchResultCallback() {

        @Override
        public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
            if (mWaitPoiSearch) {
                //详情搜结果返回，继续处理
                mWaitPoiSearch = false;
                PoiInfoEntity poiDetailInfo = null;
                if (null != searchResultEntity && AutoMapConstant.SearchType.POI_SEARCH == searchResultEntity.getMSearchType()
                        && null != searchResultEntity.getPoiList() && !searchResultEntity.getPoiList().isEmpty()) {
                    //收到poi详情搜结果，发起路线规划
                    poiDetailInfo = searchResultEntity.getMPoiList().get(0);
                }
                dealPoiDetailResult(poiDetailInfo);
                return;
            }

            //获取本次搜索关键字
            String keyword = "";
            if (null == searchResultEntity || TextUtils.isEmpty(searchResultEntity.getKeyword())) {
                Logger.d(IVrBridgeConstant.TAG, "search or keyword empty");
            } else {
                keyword = searchResultEntity.getKeyword();
            }

            //关键字和taskId是否匹配
            final boolean taskEqual = mSearchTaskId == taskId;
            final boolean keywordEqual = Objects.equals(mKeyword, keyword);
            Logger.d(IVrBridgeConstant.TAG, "taskEqual: " + taskEqual + ", keywordEqual: " + keywordEqual);
            //两个条件都不匹配，不继续执行
            if (!(taskEqual || keywordEqual)) {
                return;
            }

            mSearchTaskId = -1;
            final boolean searchSuccess;
            mSearchResultList.clear();
            if (null == searchResultEntity || null == searchResultEntity.getPoiList() || searchResultEntity.getPoiList().isEmpty()) {
                Logger.e(IVrBridgeConstant.TAG, "searchResult is empty");
                searchSuccess = false;
            } else {
                searchSuccess = true;
                mSearchResultList.addAll(searchResultEntity.getPoiList());
            }

            //根据语音搜索类型执行下一步
            switch (mSearchType) {
                case IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD:
                case IVrBridgeConstant.VoiceSearchType.WITH_PREFER:
                    //仅目的地或带偏好
                    dealSingleDestinationResult(searchSuccess);
                    break;
                case IVrBridgeConstant.VoiceSearchType.ALONG_WAY:
                    //沿途
                    dealAlongWaySearchResult();
                    break;
                case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                    //设置公司或家地址:关键字搜索结果
                    dealHomeCompanyResult(searchSuccess, false);
                    break;
                case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                    //收藏指定poi关键字搜索结果
                    if (searchSuccess) {
                        dealAddFavoriteResult();
                    } else {
                        responseSearchEmpty();
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.CONDITION_IN_PAGE:
                    //多条件搜索，判断是否还有筛选项
                    dealConditionSearchResult(searchSuccess);
                    break;
                case IVrBridgeConstant.VoiceSearchType.POI_SORT:
                    //Poi排序
                    dealSortResult();
                    break;
                case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                    //多目的地泛型poi点
                    dealMultipleDestResult(searchSuccess, true);
                    break;
                case IVrBridgeConstant.VoiceSearchType.NAVI_TO_HOME_COMPANY:
                    //导航回家/去公司，先设置地址
                    dealNaviToHomeCompanyResult(searchSuccess, false);
                    break;
                default:
                    break;
            }
        }

        @Override
        public void onSilentSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
            //静默搜索结果回调，根据内部SearchType执行后续处理
            if (mSearchTaskId != taskId) {
                return;
            }

            mSearchTaskId = -1;
            boolean searchSuccess = true;
            if (null == searchResultEntity || null == searchResultEntity.getPoiList() || searchResultEntity.getPoiList().isEmpty()) {
                Logger.e(IVrBridgeConstant.TAG, "searchResult is empty");
                searchSuccess = false;
            }
            PoiInfoEntity firstPoi = null;
            mSearchResultList.clear();
            if (searchSuccess) {
                firstPoi = searchResultEntity.getPoiList().get(0);
                mSearchResultList.addAll(searchResultEntity.getPoiList());
            }

            switch (mSearchType) {
                case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                    //多目的地普通Poi点
                    dealMultipleDestResult(searchSuccess, false);
                    break;
                case IVrBridgeConstant.VoiceSearchType.WITH_CONDITION:
                    //先搜索中心点再打开周边搜界面
                    dealConditionCenterResult(searchSuccess, firstPoi);
                    break;
                case IVrBridgeConstant.VoiceSearchType.TIME_AND_DIST:
                    //到家/公司的ETA信息预测
                    dealTimeDistResult(searchSuccess, firstPoi);
                    break;
                case IVrBridgeConstant.VoiceSearchType.START_ARRIVAL_INFO:
                    //指定poi点ETA信息，eg:xxx到xxx需要多久/大概多远
                    if (searchSuccess) {
                        mEtaPointList.add(firstPoi.getPoint());
                        processNextEtaPoint();
                    } else {
                        mEtaNameList.clear();
                        mEtaPointList.clear();
                        responseSearchEmptyWithKeyword();
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.SHOW_POI_DETAIL:
                    //查询当前位置
                    if (searchSuccess) {
                        Logger.d(IVrBridgeConstant.TAG, "showPoiInfo: " + firstPoi.getAddress()
                                + ", lon: " + firstPoi.getPoint().getLon()
                                + ", lat: " + firstPoi.getPoint().getLat());
                        showPoiDetail(firstPoi);
                    } else if (null != mRespCallback) {
                        final CallResponse poiDetailResponse = CallResponse.createFailResponse("不好意思，我定位不到你在哪里");
                        poiDetailResponse.setNeedPlayMessage(true);
                        mRespCallback.onResponse(poiDetailResponse);
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                    //收藏当前位置
                    if (searchSuccess) {
                        addCommonFavorite(firstPoi, true);
                    } else if (null != mRespCallback) {
                        final CallResponse favoriteResponse = CallResponse.createFailResponse("未查询到定位信息，无法收藏");
                        favoriteResponse.setNeedPlayMessage(true);
                        mRespCallback.onResponse(favoriteResponse);
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                    //设置家/公司为当前地址
                    dealHomeCompanyResult(searchSuccess, true);
                    break;
                case IVrBridgeConstant.VoiceSearchType.NAVI_TO_HOME_COMPANY:
                    dealNaviToHomeCompanyResult(searchSuccess, true);
                    break;
                default:
                    Logger.w(IVrBridgeConstant.TAG, "unHandle silent search: " + mSearchType);
                    break;
            }
        }

        @Override
        public void onShowStateChanged(final boolean isShow) {
            Logger.d(IVrBridgeConstant.TAG, "showSearchListPage: " + isShow);
            if (mListPageOpened == isShow) {
                return;
            }

            mListPageOpened = isShow;
            MapStateManager.getInstance().updateListPageState(mListPageOpened);
        }
    };

    /**
     * 处理NaviCommandImpl.onRouteNavi.
     *
     * @param sessionId   多轮同步id.
     * @param arrivalBean 搜索/导航目的地条件集合.
     * @param poiCallback 搜索结果回调.
     * @return CallResponse, 语音直接结果回调.
     */
    public CallResponse handleCommonSearch(final String sessionId, final ArrivalBean arrivalBean, final PoiCallback poiCallback) {
        if (null == poiCallback) {
            return CallResponse.createFailResponse("空的搜索结果回调");
        }
        mPoiCallback = poiCallback;
        if (null == arrivalBean || TextUtils.isEmpty(arrivalBean.getDest())) {
            Logger.e(IVrBridgeConstant.TAG, "voiceSearchParams is null");
            return CallResponse.createFailResponse("目的地为空");
        }

        mSessionId = sessionId;
        mDestInfo = arrivalBean;
        //目的地
        final String dest = mDestInfo.getDest();
        //路线偏好
        mRouteType = mDestInfo.getRouteType();
        //检索条件
        final Map<String, String> conditions = mDestInfo.getConditions();
        //途径点
        final SparseArray<PoiBean> passByPoi = mDestInfo.getPassbyPoi();
        //根据输入的目的地封装参数判断当前搜索/导航意图
        if (null == passByPoi || passByPoi.size() == 0) {
            if (null == conditions || conditions.isEmpty()) {
                if (null == mRouteType) {
                    //没有过滤搜索条件，直接关键字搜索
                    mSearchType = IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD;
                } else {
                    //带有算路偏好的目的地
                    mSearchType = IVrBridgeConstant.VoiceSearchType.WITH_PREFER;
                }
            } else {
                //带有筛选条件
                mSearchType = IVrBridgeConstant.VoiceSearchType.WITH_CONDITION;
            }
        } else {
            //多目的（一个目的地和一到多个途径点）
            mSearchType = IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY;
        }
        Logger.d(IVrBridgeConstant.TAG, " keywordSearch voiceInnerType: " + mSearchType);
        switch (mSearchType) {
            case IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD:
            case IVrBridgeConstant.VoiceSearchType.WITH_PREFER:
                //单目的地或带偏好
                return disposeSingleDest(dest);
            case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                //多目的地
                return disposeMultipleDest();
            case IVrBridgeConstant.VoiceSearchType.WITH_CONDITION:
                //多条件搜索，eg附近的xxx、xxx附近的xxx、xxx附近xxx的xxx
                return disposeConditionSearch();
            default:
                Logger.w(IVrBridgeConstant.TAG, "unHandle searchType: " + mSearchType);
                return CallResponse.createFailResponse("不支持的搜索/导航意图");
        }
    }

    /**
     * 处理单目的地（带偏好）的搜索/导航意图.
     *
     * @param dest String，目的地类型或关键字.
     * @return CallResponse, 语音执行结果.
     */
    private CallResponse disposeSingleDest(final String dest) {
        final int type = switch (dest) {
            case IVrBridgeConstant.DestType.HOME -> 1;
            case IVrBridgeConstant.DestType.COMPANY -> 2;
            default -> 0;
        };
        if (type == 1 || type == 2) {
            //回家或去公司需要要先结束当前导航
            if (MapStateManager.getInstance().isNaviStatus()) {
                NaviPackage.getInstance().stopNavigation();
            }
            final PoiInfoEntity homeCompanyInfo = getHomeCompanyPoiInfo(type);
            if (null != homeCompanyInfo) {
                if (!inNaviStatus()) {
                    //单目的地涉及家和公司，需要播报路线规划结果
                    mShouldPlayRouteMsg = true;
                }
                planRoute(homeCompanyInfo, null);
            } else {
                if (type == 1) {
                    return havaNoHomeAddress();
                } else {
                    return havaNoCompanyAddress();
                }
            }
        } else {
            mKeyword = dest;
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NAVING.equals(curStatus)
                    || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)
                    || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus)) {
                bundle.putBoolean("IS_END", true);
            }
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, dest);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 获取保存的家或公司信息.
     *
     * @param type 1-家  2-公司
     * @return PoiInfoEntity.
     */
    private PoiInfoEntity getHomeCompanyPoiInfo(final int type) {
        return switch (type) {
            case 1 -> BehaviorPackage.getInstance().getHomeFavoriteInfo();
            case 2 -> BehaviorPackage.getInstance().getCompanyFavoriteInfo();
            default -> null;
        };
    }

    /**
     * 根据输入的关键字跳转到搜索结果页.
     *
     * @param keyword String，搜索关键字.
     */
    private void jumpToSearchPage(final String keyword) {
        if (null == keyword || keyword.isEmpty()) {
            return;
        }

        mKeyword = keyword;
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, keyword);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 根据搜索结果处理单目的地(或带偏好)的搜索结果.
     *
     * @param success boolean，是否搜索成功.
     */
    private void dealSingleDestinationResult(final boolean success) {
        if (!success) {
            responseSearchEmpty();
            return;
        }

        final int count = mSearchResultList.size();
        if (count == 1) {
            //关键字搜结果只有一个，需要poi详情搜结果返回后再发起算路
            mWaitPoiSearch = true;
        } else {
            //搜索结果为多个，回调给语音，列表多轮选择
            responseSearchWithResult();
        }
    }

    /**
     * 搜结果唯一时触发poi详情搜后的处理.
     *
     * @param poiInfo PoiInfoEntity，poi详情信息.
     */
    private void dealPoiDetailResult(final PoiInfoEntity poiInfo) {
        if (null == poiInfo) {
            Logger.w(IVrBridgeConstant.TAG, "poiDetailSearch result is empty");
            if (null != mPoiCallback) {
                final CallResponse poiDetailResponse = CallResponse.createFailResponse("详情搜无返回结果");
                poiDetailResponse.setNeedPlayMessage(true);
                mPoiCallback.onResponse(poiDetailResponse);
            }
            return;
        }

        switch (mSearchType) {
            case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                updateHomeCompany(poiInfo);
                sendClosePage();
                break;
            case IVrBridgeConstant.VoiceSearchType.NAVI_TO_HOME_COMPANY:
                saveAndNaviToHomeCompany(poiInfo);
                break;
            case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                addCommonFavorite(poiInfo, false);
                break;
            default:
                planRoute(poiInfo, null);
                break;
        }
    }

    /**
     * 根据路线结果数目回复提示信息.
     */
    public void playRouteResult() {
        if (mShouldPlayRouteMsg) {
            mShouldPlayRouteMsg = false;
            if (null != mPoiCallback) {
                final CallResponse routeResponse = CallResponse.createSuccessResponse();
                routeResponse.setSubCallResult(NaviSubCallResult.RESP_MULTI_POI_SEARCH_SUCCESS);
                mPoiCallback.onResponse(CallResponse.createSuccessResponse());
            }
        }
    }


    /**
     * 创建未设置家地址的回复.
     *
     * @return CallResponse, 语音指令回复.
     */
    private CallResponse havaNoHomeAddress() {
        return CallResponse.createFailResponse("未找到家的地址，先去添加吧，试试说：设置家的地址");
    }

    /**
     * 创建未设置公司地址的回复.
     *
     * @return CallResponse, 语音指令回复.
     */
    private CallResponse havaNoCompanyAddress() {
        return CallResponse.createFailResponse("未找到公司的地址，先去添加吧，试试说：设置公司的地址");
    }

    /**
     * 将搜索结果回调给语音.
     */
    private void responseSearchWithResult() {
        if (null != mPoiCallback) {
            try {
                final List<PoiBean> poiBeanList = VoiceConvertUtil.convertSearchResult(mSearchResultList);
                final int size = poiBeanList.size();
                Logger.d(IVrBridgeConstant.TAG, "responseToVoice, totalSearchSize: " + size);
                mPoiCallback.onPoiSearch(mSessionId, poiBeanList, size);
            } catch (NullPointerException npe) {
                Logger.w(IVrBridgeConstant.TAG, " error: " + npe.getMessage());
            }
        }
    }

    /**
     * 回复空搜索结果.
     */
    private void responseSearchEmpty() {
        if (null != mPoiCallback) {
            final CallResponse response = CallResponse.createFailResponse("未找到相关结果，试试别的吧");
            response.setNeedPlayMessage(true);
            mPoiCallback.onResponse(response);
        }
    }

    /**
     * 带关键字回复搜索结果为空.
     */
    private void responseSearchEmptyWithKeyword() {
        if (null != mPoiCallback) {
            final CallResponse response = CallResponse.createFailResponse("搜索" + mKeyword + "结果为空，试试别的吧");
            response.setNeedPlayMessage(true);
            mPoiCallback.onResponse(response);
        }
    }

    /**
     * 根据选中目的地发起算路.
     *
     * @param endPoi  目的地Poi信息
     * @param viaList 途径点列表.
     */
    private void planRoute(final PoiInfoEntity endPoi, final List<PoiInfoEntity> viaList) {
        final RouteSpeechRequestParam requestParam = new RouteSpeechRequestParam();
        requestParam.setMEndPoiInfoEntity(endPoi);
        requestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);

        if (!(null == viaList || viaList.isEmpty())) {
            requestParam.setMViaPoiInfoEntityList(viaList);
        }

        if (!TextUtils.isEmpty(mRouteType)) {
            final RoutePreferenceID targetPrefer = VoiceConvertUtil.convertToAMapPrefer(mRouteType);
            final RoutePreferenceID curPrefer = SettingPackage.getInstance().getRoutePreference();
            if (targetPrefer != curPrefer) {
                SettingPackage.getInstance().setRoutePreference(targetPrefer);
            }
            mRouteType = null;
        }

        if (inNaviStatus()) {
            //当前为导航态，更换目的地直接发起快速导航
            RoutePackage.getInstance().requestRouteFromSpeech(requestParam);
        } else {
            //打开算路界面
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ROUTING);
            bundle.putSerializable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST, requestParam);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        }
    }

    /**
     * 判断当前是否处理引导态.
     *
     * @return true:引导态   false:非引导态.
     */
    private boolean inNaviStatus() {
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        return NaviStatus.NaviStatusType.NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus);
    }

    /**
     * 处理多目的地导航.
     *
     * @return CallResponse, 语音指令回复.
     */
    private CallResponse disposeMultipleDest() {
        //解析途径点和目的地信息
        final SparseArray<PoiBean> passByPoi = mDestInfo.getPassbyPoi();
        if (null == passByPoi || passByPoi.size() == 0) {
            Logger.w(IVrBridgeConstant.TAG, "多目的意图，途径点为空");
            return CallResponse.createFailResponse("多目的地导航途径点信息为空");
        }

        mNormalDestList.clear();
        mGenericsDestList.clear();
        mMultiplePoiArray.clear();
        mProcessDestIndex = -1;

        boolean containHome = false;
        boolean containCompany = false;
        //处理途径点
        final int size = passByPoi.size();
        Logger.d(IVrBridgeConstant.TAG, "disposeMultipleDest: passByPoiSize = " + size);
        for (int i = 0; i < size; i++) {
            final PoiBean poiBean = passByPoi.valueAt(i);
            final String name = poiBean.getName();
            final String type = poiBean.getType();
            final SingleDestInfo singleDestInfo = new SingleDestInfo();
            singleDestInfo.setDestName(name);
            singleDestInfo.setDestType(type);

            if (IVrBridgeConstant.DestType.HOME.equals(name)) {
                containHome = true;
                mNormalDestList.put(i, singleDestInfo);
            } else if (IVrBridgeConstant.DestType.COMPANY.equals(name)) {
                containCompany = true;
                mNormalDestList.put(i, singleDestInfo);
            } else if (mGenericsList.contains(type)) {
                Logger.d(IVrBridgeConstant.TAG, "process " + type);
                mGenericsDestList.put(i, singleDestInfo);
            } else {
                mNormalDestList.put(i, singleDestInfo);
            }
        }

        //添加目的地信息，并判断是否包含家、公司、泛型POI
        final SingleDestInfo singleDestInfo = new SingleDestInfo();
        final String destName = mDestInfo.getDest();
        final String destType = mDestInfo.getDestType();
        singleDestInfo.setDestName(destName);
        singleDestInfo.setDestType(destType);
        if (IVrBridgeConstant.DestType.HOME.equals(destName)) {
            containHome = true;
            mNormalDestList.put(size, singleDestInfo);
        } else if (IVrBridgeConstant.DestType.COMPANY.equals(destName)) {
            containCompany = true;
            mNormalDestList.put(size, singleDestInfo);
        } else if (mGenericsList.contains(destType)) {
            Logger.d(IVrBridgeConstant.TAG, "process " + destType);
            mGenericsDestList.put(size, singleDestInfo);
        } else {
            mNormalDestList.put(size, singleDestInfo);
        }

        //家和公司需要先判断是否已经设置，泛型POI需要依次展示搜索结果
        if (containHome) {
            final PoiInfoEntity homeInfo = getHomeCompanyPoiInfo(1);
            if (null == homeInfo) {
                Logger.w(IVrBridgeConstant.TAG, "MultipleDest contain home but info is empty");
                return havaNoHomeAddress();
            } else if (!inNaviStatus()) {
                //多目的地涉及家和公司，需要播报路线规划结果
                mShouldPlayRouteMsg = true;
            }
        }
        if (containCompany) {
            final PoiInfoEntity companyInfo = getHomeCompanyPoiInfo(2);
            if (null == companyInfo) {
                Logger.w(IVrBridgeConstant.TAG, "MultipleDest contain company but info is empty");
                return havaNoCompanyAddress();
            } else if (!inNaviStatus()) {
                //多目的地涉及家和公司，需要播报路线规划结果
                mShouldPlayRouteMsg = true;
            }
        }

        Logger.d(IVrBridgeConstant.TAG, "multipleDest normal size: " + mNormalDestList.size() + ", generics size: " + mGenericsDestList.size());
        dealNextMultipleDest(null);
        Logger.i(IVrBridgeConstant.TAG, "multiple return success response");
        final CallResponse response = CallResponse.createSuccessResponse("正在顺序执行多目的处理");
        response.setNeedPlayMessage(false);
        response.setCallResult(1);
        return response;
    }

    /**
     * 根据搜索结果执行多目的地下一步.
     *
     * @param success     是否搜索成功.
     * @param genericsPoi 是否为泛型搜索结果. true-将结果回调给语音供用户选择  false-默认选取第一个搜索结果.
     */
    private void dealMultipleDestResult(final boolean success, final boolean genericsPoi) {
        if (!success) {
            Logger.e(IVrBridgeConstant.TAG, "silentSearch has not result");
            responseSearchEmptyWithKeyword();
            return;
        }

        if (genericsPoi) {
            Logger.d(IVrBridgeConstant.TAG, "multiple dest get generics " + mKeyword + " search result");
            responseSearchWithResult();
        } else {
            dealNextMultipleDest(mSearchResultList.get(0));
        }
    }

    /**
     * 选定多目的某个dest对应的Poi信息继续执行下一个，直到泛型和非泛型Poi全部确定，发起路线规划.
     *
     * @param poiInfo PoiInfoEntity，选定的poi信息.
     */
    private void dealNextMultipleDest(final PoiInfoEntity poiInfo) {
        //保存上一轮结果
        if (null != poiInfo && mProcessDestIndex >= 0) {
            mMultiplePoiArray.put(mProcessDestIndex, poiInfo);
        }

        //继续先澄清泛型再搜索普通poi
        if (null != mGenericsDestList && mGenericsDestList.size() > 0) {
            //继续澄清下一个泛型
            mProcessDestIndex = mGenericsDestList.keyAt(0);
            Logger.d(IVrBridgeConstant.TAG, "genericsDestIndex: " + mProcessDestIndex);
            final SingleDestInfo genericsDest = mGenericsDestList.get(mProcessDestIndex);
            mKeyword = genericsDest.getDestName();
            Logger.d(IVrBridgeConstant.TAG, "genericsDestName: " + mKeyword);
            mGenericsDestList.remove(mProcessDestIndex);
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        } else if (null != mNormalDestList && mNormalDestList.size() > 0) {
            //静默搜索普通poi
            mProcessDestIndex = mNormalDestList.keyAt(0);
            Logger.d(IVrBridgeConstant.TAG, "normalDestIndex: " + mProcessDestIndex);
            final SingleDestInfo normalDest = mNormalDestList.get(mProcessDestIndex);
            Logger.d(IVrBridgeConstant.TAG, "normalDestTYPE: " + normalDest.getDestType());
            mNormalDestList.remove(mProcessDestIndex);
            if (IVrBridgeConstant.DestType.HOME.equals(normalDest.getDestType())) {
                dealNextMultipleDest(getHomeCompanyPoiInfo(1));
            } else if (IVrBridgeConstant.DestType.COMPANY.equals(normalDest.getDestType())) {
                dealNextMultipleDest(getHomeCompanyPoiInfo(2));
            } else {
                mKeyword = normalDest.getDestName();
                Logger.d(IVrBridgeConstant.TAG, "normalDestName: " + mKeyword);
                mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, mKeyword);
            }
        } else if (null != mMultiplePoiArray && mMultiplePoiArray.size() > 0) {
            //全部获取完毕，开始路线规划
            int size = mMultiplePoiArray.size();
            Logger.d(IVrBridgeConstant.TAG, "totalMultiple poi size: " + size);
            final PoiInfoEntity endPoi = mMultiplePoiArray.valueAt(size - 1);
            Logger.d(IVrBridgeConstant.TAG, "poiName: " + endPoi.getName() + ", address: " + endPoi.getAddress() + ", pid: " + endPoi.getPid());
            mMultiplePoiArray.removeAt(size - 1);

            size = mMultiplePoiArray.size();
            Logger.d(IVrBridgeConstant.TAG, "afterRemoveEnd size: " + size);
            final List<PoiInfoEntity> viaList = new ArrayList<>();
            for (int i = 0; i < size; i++) {
                viaList.add(mMultiplePoiArray.valueAt(i));
            }
            Logger.d(IVrBridgeConstant.TAG, "multipleDest viaSize:" + size);
            //规划路线
            planRoute(endPoi, viaList);
        }
    }

    /**
     * Poi多轮根据index选定Poi点.
     *
     * @param sessionId    语音多轮唯一标识.
     * @param index        搜索结果下标.
     * @param respCallback RespCallback，语音响应回调.
     */
    public void handlePoiSelectIndex(final String sessionId, final int index, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "selectPoiIndex: " + index);
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            responsePreviousSearchEmpty(respCallback);
            return;
        }

        if (mSearchResultList.size() > index) {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(index);
            if (null != poiInfoEntity) {
                disposeSelectedPoi(poiInfoEntity, respCallback);
            }
        } else {
            if (null != respCallback) {
                final CallResponse callResponse = CallResponse.createFailResponse("超出选择范围");
                callResponse.setNeedPlayMessage(true);
                respCallback.onResponse(callResponse);
            }
        }
    }

    /**
     * 响应找不到上一次搜索结果.
     *
     * @param respCallback RespCallback.
     */
    private void responsePreviousSearchEmpty(final RespCallback respCallback) {
        Logger.e(IVrBridgeConstant.TAG, "last searchResult is empty or index overview");
        if (null != respCallback) {
            final CallResponse callResponse = CallResponse.createFailResponse("上一轮搜索结果为空");
            callResponse.setNeedPlayMessage(true);
            respCallback.onResponse(callResponse);
        }
    }

    /**
     * 回复两轮对话标识不匹配.
     *
     * @param respCallback RespCallback，语音执行结果回调.
     */
    private void responseNotMatchId(final RespCallback respCallback) {
        Logger.e(IVrBridgeConstant.TAG, "last sessionId is empty or not matching");
        if (null != respCallback) {
            final CallResponse response = CallResponse.createFailResponse("sessionId不匹配");
            response.setNeedPlayMessage(true);
            respCallback.onResponse(response);
        }
    }

    /**
     * 根据条件选择搜索结果列表某一项.
     *
     * @param sessionId    语音多轮唯一标识.
     * @param rule         1-最近的  2-评分最高的  3-价格最低  4-价格最高.
     * @param respCallback 语音传入的结果回调接口.
     */
    public void handlePoiSelectRule(final String sessionId, final int rule, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "selectPoi with rule: " + rule);
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            responsePreviousSearchEmpty(respCallback);
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
                Logger.e(IVrBridgeConstant.TAG, "not handle rule: " + rule);
                break;
        }
    }

    /**
     * 选择搜索结果中距离最近的poi点.
     *
     * @param respCallback RespCallback.
     */
    private void chooseNearestPoi(final RespCallback respCallback) {
        final int size = mSearchResultList.size();
        int distance = 0;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }
            final int sortDistance = poiInfoEntity.getSort_distance();
            if (distance <= 0 || distance > sortDistance) {
                distance = sortDistance;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    /**
     * 选择评分最高的Poi点.
     *
     * @param respCallback RespCallback.
     */
    private void chooseHighestPoi(final RespCallback respCallback) {
        final int size = mSearchResultList.size();
        int rate = -1;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }
            final int sortRate = poiInfoEntity.getSort_rate();
            if (rate < 0 || sortRate > rate) {
                rate = sortRate;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    /**
     * 选择价格最低Poi点.
     *
     * @param respCallback RespCallback，语音响应回调类.
     */
    private void chooseCheapestPoi(final RespCallback respCallback) {
        final int size = mSearchResultList.size();
        int price = -1;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }

            final int sortPrice = poiInfoEntity.getSort_price();
            if (price < 0 || sortPrice < price) {
                price = sortPrice;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    /**
     * 选择价格最高Poi点.
     *
     * @param respCallback 语音执行结果回调.
     */
    private void chooseDearestPoi(final RespCallback respCallback) {
        final int size = mSearchResultList.size();
        int price = -1;
        PoiInfoEntity targetPoi = null;
        for (int i = 0; i < size; i++) {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(i);
            if (null == poiInfoEntity) {
                continue;
            }

            final int sortPrice = poiInfoEntity.getSort_price();
            if (price < 0 || sortPrice > price) {
                price = sortPrice;
                targetPoi = poiInfoEntity;
            }
        }

        if (null != targetPoi) {
            disposeSelectedPoi(targetPoi, respCallback);
        }
    }

    /**
     * 处理最后选中的Poi.
     *
     * @param poiInfo      PoiInfoEntity，选定的Poi信息
     * @param respCallback 语音响应回调.
     */
    private void disposeSelectedPoi(final PoiInfoEntity poiInfo, final RespCallback respCallback) {
        mSearchResultList.clear();

        switch (mSearchType) {
            case IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY:
                //设置家/公司地址为选择的poi
                updateHomeCompany(poiInfo);
                sendClosePage();
                break;
            case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                //选中的点为泛型poi，继续处理下一个点
                dealNextMultipleDest(poiInfo);
                break;
            case IVrBridgeConstant.VoiceSearchType.ALONG_WAY:
                //选中poi作为途径点
                mAlongToAround = false;
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfo);
                break;
            case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                addCommonFavorite(poiInfo, false);
                sendClosePage();
                break;
            case IVrBridgeConstant.VoiceSearchType.NAVI_TO_HOME_COMPANY:
                saveAndNaviToHomeCompany(poiInfo);
                break;
            default:
                //所选poi作为目的地发起算路
                planRoute(poiInfo, null);
                break;
        }
    }


    /**
     * 发送指令，关闭当前HMI界面，如设置家/公司、途径点选择等.
     */
    private void sendClosePage() {
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.CLOSE_CURRENT_PAGE);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 发送指令，应用退到后台.
     */
    public void sendMoveBack() {
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.MOVE_TO_BACK);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 带条件的目的地检索.
     *
     * @return CallResponse, 语音指令回复.
     */
    private CallResponse disposeConditionSearch() {
        final Map<String, String> conditionMap = mDestInfo.getConditions();
        if (null == conditionMap || conditionMap.isEmpty()) {
            Logger.d(IVrBridgeConstant.TAG, "conditionSearch map is empty");
            return CallResponse.createFailResponse("检索条件为空");
        }

        mSortValue = null;
        mSearchCondition = new VoiceSearchConditions();
        mSearchCondition.parseConditionMap(conditionMap);
        Logger.d(IVrBridgeConstant.TAG, "afterParse, conditions: " + mSearchCondition);
        //等级，类似5A/4A/3A/2A/1A/5星/4星/3星/2星/1星，如果存在，和dest一起拼接成keyword
        final String level = mSearchCondition.getLevel();
        if (!TextUtils.isEmpty(level)) {
            mKeyword = level + mDestInfo.getDest();
        } else {
            mKeyword = mDestInfo.getDest();
        }

        final String distance = mSearchCondition.getDistance(); //距离
        final String price = mSearchCondition.getPrice(); //价格
        final String rate = mSearchCondition.getRate(); //评分
        if (IVrBridgeConstant.DistanceValue.NEAREST.equals(distance)) {
            //带有距离优先筛选
            mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_DISTANCE;
        } else if (IVrBridgeConstant.PRICE_CHEAP.equals(price)) {
            mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_LOW_PRICE;
        } else if (IVrBridgeConstant.RATE_HIGH.equals(rate)) {
            mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_RATE;
        } else {
            mSortValue = "";
        }

        final String center = mSearchCondition.getCenter(); //中心点，没有则为当前位置
        if (!TextUtils.isEmpty(center)) {
            //先搜索中心点
            mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, center);
        } else {
            if (!(VoiceConvertUtil.isNumber(distance))) {
                //执行关键字搜索
                mSearchType = IVrBridgeConstant.VoiceSearchType.CONDITION_IN_PAGE;
                final Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
                bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            } else {
                //没有中心点，当前位置的周边搜
                final LocInfoBean curLocation = PositionPackage.getInstance().getLastCarLocation();
                if (null != curLocation) {
                    final GeoPoint geoPoint = new GeoPoint(curLocation.getLongitude(), curLocation.getLatitude());
                    dealAfterConditionCenter(geoPoint);
                } else {
                    return CallResponse.createFailResponse("无法获取当前定位信息，请稍后重试");
                }
            }
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理多条件搜索中心点搜索结果.
     *
     * @param success    是否搜索成功
     * @param centerInfo 搜索中心点Poi信息.
     */
    private void dealConditionCenterResult(final boolean success, final PoiInfoEntity centerInfo) {
        if (!success && null == centerInfo || null == centerInfo.getPoint()) {
            Logger.e(IVrBridgeConstant.TAG, "searchAround center is empty");
            mKeyword = mSearchCondition.getCenter();
            responseSearchEmptyWithKeyword();
            return;
        }

        final GeoPoint geoPoint = centerInfo.getPoint();
        dealAfterConditionCenter(geoPoint);
    }

    /**
     * 获取中心点坐标后继续处理多条件搜索.
     *
     * @param geoPoint GeoPoint,中心点坐标.
     */
    private void dealAfterConditionCenter(final GeoPoint geoPoint) {
        if (null == geoPoint || null == mKeyword) {
            return;
        }

        mSearchType = IVrBridgeConstant.VoiceSearchType.CONDITION_IN_PAGE;
        final String distance = mSearchCondition.getDistance();
        int radius = VoiceConvertUtil.getIntValue(distance);
        radius = radius > 0 ? radius : 5000;
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.AROUND_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
        bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.AROUND_POINT, geoPoint);
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.AROUND_RADIUS, radius);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 处理多条件搜索结果回调.
     *
     * @param success true-成功 false-没有搜索结果.
     */
    private void dealConditionSearchResult(final boolean success) {
        if (!success) {
            responseSearchEmpty();
            return;
        }

        if (!TextUtils.isEmpty(mSortValue)) {
            //继续执行条件筛选
            final String sortValue = mSortValue;
            mSortValue = "";
            SearchPackage.getInstance().voiceSortPoi(MapType.MAIN_SCREEN_MAIN_MAP, sortValue);
        } else {
            responseSearchWithResult();
        }
    }

    /**
     * 处理沿途搜索逻辑.
     *
     * @param sessionId   语音多轮唯一标识.
     * @param passBy      关键字.
     * @param poiType     沿途点类型.
     * @param poiCallback 执行结果回调.
     */
    public void handlePassBy(final String sessionId, final String passBy, final String poiType, final PoiCallback poiCallback) {
        mAlongToAround = false;
        mSessionId = sessionId;
        mPoiCallback = poiCallback;
        //保存沿途搜参数，如果沿途搜没有结果，需转为周边搜
        mKeyword = passBy;
        mSearchType = IVrBridgeConstant.VoiceSearchType.ALONG_WAY;
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ALONG_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, passBy);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 处理沿途搜结果，一个或多个结果都需要用户确认添加途径点.
     */
    private void dealAlongWaySearchResult() {
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            if (mAlongToAround) {
                //已经触发沿途搜转周边
                mAlongToAround = false;
                Logger.d(IVrBridgeConstant.TAG, "AlongSearch turnAround: " + mKeyword + ", result is empty");
                responseSearchEmpty();
            } else {
                mAlongToAround = true;
                Logger.d(IVrBridgeConstant.TAG, "AlongSearch " + mKeyword + "result is empty, turnAroundSearch");
                final LocInfoBean locInfo = PositionPackage.getInstance().getLastCarLocation();
                if (null != locInfo) {
                    final GeoPoint geoPoint = new GeoPoint(locInfo.getLongitude(), locInfo.getLatitude());
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.AROUND_SEARCH);
                    bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
                    bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.AROUND_POINT, geoPoint);
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.AROUND_RADIUS, 5000);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                } else {
                    responseSearchEmpty();
                }
            }
            return;
        }

        responseSearchWithResult();
    }

    /**
     * 设置公司/家的地址.
     *
     * @param sessionId   String，多轮对话保持一致性
     * @param poiType     String，HOME-家，COMPANY-公司
     * @param poi         String，CURRENT_LOCATION-当前地址 or poi名称
     * @param poiCallback PoiCallback，搜索结果回调.
     * @return CallResponse, 语音指令回复.
     */
    public CallResponse setHomeCompany(final String sessionId, final String poiType, final String poi, final PoiCallback poiCallback) {
        mSessionId = sessionId;
        mPoiType = poiType;
        mPoiCallback = poiCallback;
        mSearchType = IVrBridgeConstant.VoiceSearchType.SET_HOME_COMPANY;
        if (IVrBridgeConstant.CURRENT_LOCATION.equals(poi)) {
            //当前地址，逆地理搜索
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                final GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint, true);
            } else {
                return CallResponse.createFailResponse("无法获取当前位置，请稍后重试");
            }
        } else {
            //打开设置公司/家的页面，并传入关键字发起搜索
            final int type = switch (mPoiType) {
                case IVrBridgeConstant.DestType.HOME -> 1;
                case IVrBridgeConstant.DestType.COMPANY -> 2;
                default -> 0;
            };

            if (type > 0) {
                mKeyword = poi;
                final Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.HOME_COMPANY_SET);
                bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, poi);
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.HOME_COMPANY_TYPE, type);
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            } else {
                return CallResponse.createFailResponse("不支持的设置类型");
            }
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 导航回家/去公司，地址未设置.
     *
     * @param sessionId   String，多轮对话保持一致性.
     * @param poiType     导航意图 NAVI_TO_HOME:回家
     *                    NAVI_TO_COMPANY:去公司
     * @param poi         需要设置的目的地名称.
     * @param poiCallback 语音指令回复.
     */
    public CallResponse naviToHomeCompany(final String sessionId, final String poiType, final String poi, final PoiCallback poiCallback) {
        mSessionId = sessionId;
        mPoiType = poiType;
        mPoiCallback = poiCallback;
        mSearchType = IVrBridgeConstant.VoiceSearchType.NAVI_TO_HOME_COMPANY;
        if (IVrBridgeConstant.CURRENT_LOCATION.equals(poi)) {
            //当前地址，逆地理搜索
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                final GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint, true);
            } else {
                return CallResponse.createFailResponse("无法获取当前位置，请稍后重试");
            }
        } else {
            //打开设置公司/家的页面，并传入关键字发起搜索
            final int type = switch (mPoiType) {
                case IVrBridgeConstant.DestType.NAVI_TO_HOME -> 1;
                case IVrBridgeConstant.DestType.NAVI_TO_COMPANY -> 2;
                default -> 0;
            };

            if (type > 0) {
                mKeyword = poi;
                final Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.HOME_COMPANY_SET);
                bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, poi);
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.HOME_COMPANY_TYPE, type);
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            } else {
                return CallResponse.createFailResponse("不支持的设置类型");
            }
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 根据搜索结果设置家/公司地址.
     *
     * @param searchSuccess 是否搜索成功.
     * @param geoSearch     是否为逆地理搜索  true：逆地理当前位置,静默搜不展示界面
     *                      false：关键字搜索会展示搜索结果，结果只有一个会跳转到poi详情
     */
    private void dealHomeCompanyResult(final boolean searchSuccess, final boolean geoSearch) {
        if (!searchSuccess) {
            Logger.w(IVrBridgeConstant.TAG, "setHomeCompany, searchResult is empty");
            responseSearchEmpty();
            return;
        }

        final int size = mSearchResultList.size();
        Logger.w(IVrBridgeConstant.TAG, "setHomeCompany searchResultSize:" + size + ", geoSearch:" + geoSearch);
        if (size == 1) {
            if (geoSearch) {
                updateHomeCompany(mSearchResultList.get(0));
            } else {
                // 非逆地理搜结果只有一个，需要等待poi详情搜结果返回再处理
                mWaitPoiSearch = true;
            }
        } else {
            responseSearchWithResult();
        }
    }

    /**
     * 根据选定的Poi设置或更新家-公司信息.
     *
     * @param poiInfo PoiInfoEntity，选择的Poi信息.
     */
    private void updateHomeCompany(final PoiInfoEntity poiInfo) {
        if (null == poiInfo || null == poiInfo.getPid()) {
            return;
        }

        Logger.w(IVrBridgeConstant.TAG, "setHomeCompany poiName: " + poiInfo.getName());
        final int type;
        final String tts;
        if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
            //家
            type = 1;
            tts = "已设置家的地址";
        } else {
            //公司
            type = 2;
            tts = "已设置公司的地址";
        }

        if (null != mPoiCallback) {
            final CallResponse homeCompanyResponse = CallResponse.createSuccessResponse(tts);
            homeCompanyResponse.setNeedPlayMessage(true);
            mPoiCallback.onResponse(homeCompanyResponse);
        }

        saveHomeCompany(poiInfo, type);
    }

    /**
     * 保存家/公司信息.
     *
     * @param poiInfo 地址信息.
     * @param type    类型  1:家  2:公司.
     */
    private void saveHomeCompany(final PoiInfoEntity poiInfo, final int type) {
        final FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(type);
        poiInfo.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavorite(poiInfo, type);
    }

    /**
     * 根据搜索结果处理导航到家/公司.
     *
     * @param searchSuccess true:搜索成功   false:搜索无结果.
     * @param geoSearch     true:逆地理搜索，当前位置  false:关键字搜索.
     */
    private void dealNaviToHomeCompanyResult(final boolean searchSuccess, final boolean geoSearch) {
        if (!searchSuccess) {
            Logger.w(IVrBridgeConstant.TAG, "naviToHomeCompany, searchResult is empty");
            responseSearchEmpty();
            return;
        }

        final int size = mSearchResultList.size();
        Logger.w(IVrBridgeConstant.TAG, "naviToHomeCompany searchResultSize:" + size + ", geoSearch:" + geoSearch);
        if (size == 1) {
            if (geoSearch) {
                saveAndNaviToHomeCompany(mSearchResultList.get(0));
            } else {
                // 非逆地理搜结果只有一个，需要等待poi详情搜结果返回再处理
                mWaitPoiSearch = true;
            }
        } else {
            responseSearchWithResult();
        }
    }

    /**
     * 保存家/公司信息并导航到此地址.
     *
     * @param poiInfo 搜索结果信息.
     */
    private void saveAndNaviToHomeCompany(final PoiInfoEntity poiInfo) {
        final int type;
        if (IVrBridgeConstant.DestType.NAVI_TO_HOME.equals(mPoiType)) {
            type = 1;
        } else {
            type = 2;
        }
        saveHomeCompany(poiInfo, type);
        if (!inNaviStatus()) {
            //单目的地涉及家和公司，需要播报路线规划结果
            mShouldPlayRouteMsg = true;
        }
        planRoute(poiInfo, null);
    }

    /**
     * 通过逆地理搜索查询当前位置详细信息.
     *
     * @param searchType   查询当前位置的目的
     * @param geoPoint     当前定位信息
     * @param respCallback 语音响应回调.
     */
    public void queryCurrentLocationDetail(final int searchType, final GeoPoint geoPoint, final RespCallback respCallback) {
        mSearchType = searchType;
        mRespCallback = respCallback;
        mKeyword = null;
        //使用静默搜索，只获取当前位置，不展示POI详情
        mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint, true);
    }

    /**
     * 根据逆地理搜索结果展示Poi详情.
     *
     * @param poiInfo 搜索获取的当前位置Poi信息.
     */
    private void showPoiDetail(final PoiInfoEntity poiInfo) {
        if (null != poiInfo) {
            final String address = poiInfo.getAddress();
            if (null != mRespCallback) {
                final CallResponse locationResponse = CallResponse.createSuccessResponse("当前定位" + address + "附近");
                locationResponse.setNeedPlayMessage(true);
                mRespCallback.onResponse(locationResponse);
            }

            final String curState = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NO_STATUS.equals(curState)) {
                //在地图首页，打开Poi详情页面
                final Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.POI_DETAIL);
                bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.POI_DETAIL_INFO, poiInfo);
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            }
        }
    }

    /**
     * 收藏普通点.
     *
     * @param poiInfo   PoiInfoEntity，POI信息.
     * @param geoSearch true:逆地理搜索，收藏当前位置
     *                  false:关键字搜索，收藏指定poi
     */
    private void addCommonFavorite(final PoiInfoEntity poiInfo, final boolean geoSearch) {
        if (null != poiInfo) {
            Logger.d(IVrBridgeConstant.TAG, "current name: " + poiInfo.getName()
                    + ", address: " + poiInfo.getAddress()
                    + ",lon: " + poiInfo.getPoint().getLon()
                    + ", lat: " + poiInfo.getPoint().getLat());

            final CallResponse favoriteResponse = CallResponse.createSuccessResponse("已为你收藏" + poiInfo.getName());
            favoriteResponse.setNeedPlayMessage(true);
            if (geoSearch && null != mRespCallback) {
                mRespCallback.onResponse(favoriteResponse);
            } else if (null != mPoiCallback) {
                mPoiCallback.onResponse(favoriteResponse);
            }

            final FavoriteInfo favoriteInfo = new FavoriteInfo();
            favoriteInfo.setCommonName(0);
            poiInfo.setFavoriteInfo(favoriteInfo);
            BehaviorPackage.getInstance().addFavorite(poiInfo, 0);
        }
    }

    /**
     * 关键字搜索对应收藏.
     *
     * @param keyword     关键字.
     * @param poiCallback 语音搜索结果响应.
     * @return CallResponse 语音指令执行结果.
     */
    public CallResponse searchForFavorite(final String keyword, final PoiCallback poiCallback) {
        if (TextUtils.isEmpty(keyword)) {
            return CallResponse.createFailResponse("空的poi名称，无法收藏");
        }
        mSearchType = IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE;
        mPoiCallback = poiCallback;
        mKeyword = keyword;
        mSearchTaskId = -1;
        //跳转收藏，不同目的对应不同参数，分开实现
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.COLLECT_COMMON);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, keyword);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理收藏关键字结果搜索.
     */
    private void dealAddFavoriteResult() {
        final int size = mSearchResultList.size();
        if (size == 1) {
            //关键字搜索只有一个结果，等待详情搜结果展示
            mWaitPoiSearch = true;
        } else {
            responseSearchWithResult();
        }
    }

    /**
     * 搜索起点位置，默认选择第一个，计算到家/公司的TimeAndDist信息.
     *
     * @param searchType   语音搜索目的
     * @param poiType      终点类型, HOME-家  COMPANY-公司.
     * @param keyword      关键字.
     * @param respCallback 语音响应回调.
     */
    public void searchPoiInfo(final int searchType, final String poiType, final String keyword, final RespCallback respCallback) {
        if (TextUtils.isEmpty(keyword)) {
            return;
        }
        mSearchType = searchType;
        mPoiType = poiType;
        mKeyword = keyword;
        mRespCallback = respCallback;
        mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, keyword);
    }

    /**
     * 根据搜索结果，获取到家/公司的TIME_DIST信息.
     *
     * @param success 是否搜索成功.
     * @param poiInfo Poi信息.
     */
    private void dealTimeDistResult(final boolean success, final PoiInfoEntity poiInfo) {
        if (!success || null == poiInfo || null == poiInfo.getPoint()) {
            Logger.w(IVrBridgeConstant.TAG, "searchForEta empty result");
            if (null != mRespCallback) {
                final CallResponse etaResponse = CallResponse.createFailResponse("未找到" + mKeyword + "，试试别的吧");
                etaResponse.setNeedPlayMessage(true);
                mRespCallback.onResponse(etaResponse);
            }
            return;
        }

        final GeoPoint startPoint = poiInfo.getPoint();
        GeoPoint endPoint = null;
        if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
            //家
            final PoiInfoEntity homeInfo = BehaviorPackage.getInstance().getHomeFavoriteInfo();
            if (null != homeInfo && null != homeInfo.getPoint()) {
                endPoint = homeInfo.getPoint();
            }
        } else {
            //公司
            final PoiInfoEntity companyInfo = BehaviorPackage.getInstance().getCompanyFavoriteInfo();
            if (null != companyInfo && null != companyInfo.getPoint()) {
                endPoint = companyInfo.getPoint();
            }
        }

        if (null != endPoint) {
            RoutePackage.getInstance().getTravelTimeFuture(startPoint, endPoint).thenAccept(pair -> {
                final String distance = pair.first;
                final String time = pair.second;
                final StringBuilder response = new StringBuilder();
                if (!TextUtils.isEmpty(mKeyword)) {
                    response.append(mKeyword);
                }
                if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
                    response.append("到家有");
                } else {
                    response.append("到公司有");
                }
                response.append(distance).append("，大约需要").append(time);
                final String homeCompanyEta = response.toString();
                Logger.d(IVrBridgeConstant.TAG, "homeCompanyEta: " + homeCompanyEta);
                if (null != mRespCallback) {
                    final CallResponse homeCompanyEtaResponse = CallResponse.createSuccessResponse(homeCompanyEta);
                    homeCompanyEtaResponse.setNeedPlayMessage(true);
                    mRespCallback.onResponse(homeCompanyEtaResponse);
                }
            }).exceptionally(throwable -> {
                responseHaveNoInfo();
                return null;
            });
        }
    }

    /**
     * 统一回复未查找到响应信息.
     */
    private void responseHaveNoInfo() {
        if (null != mRespCallback) {
            final CallResponse noneMsgResponse = CallResponse.createFailResponse("暂未查询到相应信息，请稍后再试");
            noneMsgResponse.setNeedPlayMessage(true);
            mRespCallback.onResponse(noneMsgResponse);
        }
    }

    /**
     * 获取起点到终点的预计耗时和距离.
     *
     * @param start        起点，可以为null，意为当前位置.
     * @param arrival      终点，不可为empty.
     * @param respCallback 语音执行结果异步回调.
     * @return CallResponse, 语音执行结果同步返回.
     */
    public CallResponse getTwoPoiEtaInfo(final String start, final String arrival, final RespCallback respCallback) {
        final boolean networkStatus = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        if (!networkStatus) {
            return CallResponse.createFailResponse("当前网路状态不佳，请稍后再试");
        }
        mEtaNameList.clear();
        mEtaPointList.clear();
        if (null == start || start.isEmpty()) {
            //起点为空，获取当前信息
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            if (null != locInfoBean) {
                final GeoPoint startPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                mEtaPointList.add(startPoint);
                mEtaNameList.add("");
                mEtaNameList.add(arrival);
            } else {
                return CallResponse.createFailResponse("无法获取当前定位信息，请稍后重试");
            }
        } else {
            mEtaNameList.add(start);
            mEtaNameList.add(arrival);
        }

        mRespCallback = respCallback;
        processNextEtaPoint();
        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理两点之前ETA信息的下一个点，获取完毕后计算时长和距离.
     */
    private void processNextEtaPoint() {
        final int size = mEtaPointList.size();
        if (size == 2) { //两点信息都已获取
            RoutePackage.getInstance().getTravelTimeFuture(mEtaPointList.get(0), mEtaPointList.get(1)).thenAccept(pair -> {
                final StringBuilder response = new StringBuilder();
                final String start = mEtaNameList.get(0);
                if (!(null == start || start.isEmpty())) {
                    response.append(start);
                }
                response.append("到").append(mEtaNameList.get(1)).append("有")
                        .append(pair.first).append("，大约需要").append(pair.second);
                final String twoPointEta = response.toString();
                Logger.d(IVrBridgeConstant.TAG, "twoPoiEtaInfo: " + twoPointEta);
                if (null != mRespCallback) {
                    final CallResponse etaResponse = CallResponse.createSuccessResponse(twoPointEta);
                    etaResponse.setNeedPlayMessage(true);
                    mRespCallback.onResponse(etaResponse);
                }
            }).exceptionally(throwable -> {
                responseHaveNoInfo();
                return null;
            });
        } else {
            mSearchType = IVrBridgeConstant.VoiceSearchType.START_ARRIVAL_INFO;
            mKeyword = mEtaNameList.get(size);
            mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, mKeyword);
        }
    }

    /**
     * Poi排序.
     *
     * @param sessionId    String，语音多轮对话保持一致性.
     * @param type         排序类别.
     * @param rule         排序方式.
     * @param respCallback RespCallback，执行结果回调.
     */
    public void sortPoi(final String sessionId, final String type, final String rule, final RespCallback respCallback) {
        Logger.d(IVrBridgeConstant.TAG, "poiSort rule: " + rule);
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            responsePreviousSearchEmpty(respCallback);
            Logger.d(IVrBridgeConstant.TAG, "poiSort searchResult is empty");
            return;
        }

        mSortValue = "";
        switch (type) {
            case IVrBridgeConstant.PoiSortType.DISTANCE:
                //距离，从近到远
                if (IVrBridgeConstant.PoiSortRule.ASCENDING.equals(rule)) {
                    mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_DISTANCE;
                }
                break;
            case IVrBridgeConstant.PoiSortType.PRIZE:
                //价格，支持低价优先与高价优先
                if (IVrBridgeConstant.PoiSortRule.ASCENDING.equals(rule)) {
                    mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_LOW_PRICE;
                } else if (IVrBridgeConstant.PoiSortRule.DESCENDING.equals(rule)) {
                    mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_HIGH_PRICE;
                }
                break;
            case IVrBridgeConstant.PoiSortType.RATE:
                //评分，从高到低
                if (IVrBridgeConstant.PoiSortRule.DESCENDING.equals(rule)) {
                    mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_RATE;
                }
                break;
            default:
                Logger.d(IVrBridgeConstant.TAG, "unKnown sortType");
                break;
        }

        if (TextUtils.isEmpty(mSortValue)) {
            responseUnSupportSortRule(respCallback);
        } else {
            mSearchType = IVrBridgeConstant.VoiceSearchType.POI_SORT;
            mRespCallback = respCallback;
            SearchPackage.getInstance().voiceSortPoi(MapType.MAIN_SCREEN_MAIN_MAP, mSortValue);
        }
    }

    /**
     * 回复不支持当前筛选项.
     *
     * @param respCallback RespCallback,语音值结果回调.
     */
    private void responseUnSupportSortRule(final RespCallback respCallback) {
        if (null != respCallback) {
            final CallResponse callResponse = CallResponse.createNotSupportResponse("不支持该排序，请继续选择");
            callResponse.setNeedPlayMessage(true);
            respCallback.onResponse(callResponse);
        }
    }

    /**
     * 处理排序筛选搜索结果.
     */
    private void dealSortResult() {
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            Logger.d(IVrBridgeConstant.TAG, "poiSort type: " + mSortValue + " search result empty");
            if (null != mRespCallback) {
                final CallResponse sortResponse = CallResponse.createFailResponse("排序搜索结果为空");
                sortResponse.setNeedPlayMessage(true);
                mRespCallback.onResponse(sortResponse);
            }
            return;
        }

        final PoiInfoEntity firstPoi = mSearchResultList.get(0);
        final String name = firstPoi.getName();
        final StringBuilder builder = new StringBuilder();
        boolean appendHint = false;
        switch (mSortValue) {
            case IVrBridgeConstant.PoiSortValue.PRIORITY_DISTANCE:
                builder.append("已按距离排序，最近的");
                appendHint = true;
                break;
            case IVrBridgeConstant.PoiSortValue.PRIORITY_LOW_PRICE:
                builder.append("已按价格排序，价格最低的");
                appendHint = true;
                break;
            case IVrBridgeConstant.PoiSortValue.PRIORITY_HIGH_PRICE:
                builder.append("已按价格排序，价格最高的");
                appendHint = true;
                break;
            case IVrBridgeConstant.PoiSortValue.PRIORITY_RATE:
                builder.append("已按评分排序，评分最高的");
                appendHint = true;
                break;
            case IVrBridgeConstant.PoiSortValue.PRIORITY_RECOMMEND:
                builder.append("已使用综合排序，推荐").append(name)
                        .append("，确定去这里吗");
                break;
            default:
                Logger.i(IVrBridgeConstant.TAG, "unSupport sortValue: " + mSortValue);
                break;
        }

        if (appendHint) {
            final String distance = firstPoi.getDistance();
            builder.append(name).append("距您").append(distance).append("，去这里可以吗");
        }

        if (null != mRespCallback) {
            final String hintMsg = builder.toString();
            final CallResponse sortResultResponse;
            if (TextUtils.isEmpty(hintMsg)) {
                sortResultResponse = CallResponse.createFailResponse("不支持的排序类型");
            } else {
                sortResultResponse = CallResponse.createSuccessResponse(hintMsg);
            }
            sortResultResponse.setNeedPlayMessage(true);
            mRespCallback.onResponse(sortResultResponse);
        }
    }

}
