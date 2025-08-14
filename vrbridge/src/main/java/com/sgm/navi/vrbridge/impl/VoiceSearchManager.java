package com.sgm.navi.vrbridge.impl;

import android.os.Bundle;
import android.text.TextUtils;
import android.util.SparseArray;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.StringUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.PoiBean;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.baidu.oneos.protocol.result.NaviSubCallResult;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.vrbridge.IVrBridgeConstant;
import com.sgm.navi.vrbridge.MapStateManager;
import com.sgm.navi.vrbridge.VoiceConvertUtil;
import com.sgm.navi.vrbridge.bean.SingleDestInfo;
import com.sgm.navi.vrbridge.bean.VoiceSearchConditions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

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
    private int mMaxPage = 0;
    private int mCurrentPage = 0;
    private boolean mInTurnRound = false;
    private RespCallback mTurnCallback;
    private boolean mWaitPoiSearch = false; //搜索结果只有一个，需要poi详情搜结果返回后再执行算路
    private boolean mSupportSortPoi = false; //搜索结果是否支持排序

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

    private int mPlanRouteResult; //是否应该播报路线规划结果

    private GeoPoint mCenterPoint; //中心点坐标


    public static VoiceSearchManager getInstance() {
        return VoiceSearchManagerHolder.INSTANCE;
    }

    private static final class VoiceSearchManagerHolder {
        private static final VoiceSearchManager INSTANCE = new VoiceSearchManager();
    }

    private VoiceSearchManager() {
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

        mEtaNameList = new ArrayList<>();
        mEtaPointList = new ArrayList<>();

        mListPageOpened = SearchPackage.getInstance().isMIsShow();
        SearchPackage.getInstance().registerCallBack(IVrBridgeConstant.TAG, mSearchCallback);

        resetSearchAbout();
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

    /**
     * 每次指令执行前将相关属性重置为默认.
     */
    private void resetSearchAbout() {
        mSearchTaskId = -1;
        mSearchType = IVrBridgeConstant.VoiceSearchType.DEFAULT;
        mKeyword = null;
        mSearchResultList = new ArrayList<>();
        mMaxPage = 0;
        mCurrentPage = 0;
        mNormalDestList = new SparseArray<>();
        mGenericsDestList = new SparseArray<>();
        mMultiplePoiArray = new SparseArray<>();
        mCenterPoint = null;
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
            String keyword;
            if (null == searchResultEntity || TextUtils.isEmpty(searchResultEntity.getKeyword())) {
                keyword = "";
            } else {
                keyword = searchResultEntity.getKeyword();
            }

            //关键字和taskId是否匹配
            final boolean taskEqual = mSearchTaskId == taskId;
            boolean keywordEqual = false;
            if (!TextUtils.isEmpty(mKeyword)) {
                keywordEqual = Objects.equals(mKeyword, keyword) || keyword.contains(mKeyword);
            }
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "taskEqual: ", taskEqual, ", keywordEqual: ", keywordEqual);
            }
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
                mSupportSortPoi = false;
                mMaxPage = 0;
                mCurrentPage = 0;
            } else {
                searchSuccess = true;
                mSearchResultList.addAll(searchResultEntity.getPoiList());
                mMaxPage = searchResultEntity.getMaxPageNum();
                mCurrentPage = searchResultEntity.getPageNum();
                mSupportSortPoi = !(null == searchResultEntity.getLocalInfoList() || searchResultEntity.getLocalInfoList().isEmpty());
            }

            if (mInTurnRound) {
                //翻页搜索结果
                dealTurnPageResult(searchSuccess);
                return;
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
            mSupportSortPoi = false;

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
                        if(Logger.openLog) {
                            Logger.d(IVrBridgeConstant.TAG, "showPoiInfo: ", firstPoi.getAddress()
                                    , ", lon: ", firstPoi.getPoint().getLon()
                                    , ", lat: ", firstPoi.getPoint().getLat());
                        }
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
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "showSearchListPage: ", isShow);
            }
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.EMPTY_SEARCH_CALLBACK);
        }
        if (null == arrivalBean || TextUtils.isEmpty(arrivalBean.getDest())) {
            Logger.e(IVrBridgeConstant.TAG, "voiceSearchParams is null");
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.EMPTY_DEST);
        }

        mPoiCallback = poiCallback;
        resetSearchAbout();
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
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, " keywordSearch voiceInnerType: ", mSearchType);
        }
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
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_INTENTION);
        }
    }

    /**
     * 处理单目的地（带偏好）的搜索/导航意图.
     *
     * @param dest String，目的地类型或关键字.
     * @return CallResponse, 语音执行结果.
     */
    private CallResponse disposeSingleDest(final String dest) {
        final int type = getDestType(dest);
        if (type == 1 || type == 2) {
            final PoiInfoEntity homeCompanyInfo = getHomeCompanyPoiInfo(type);
            if (null != homeCompanyInfo) {
                //单目的地涉及家和公司，需要播报路线规划结果
                mPlanRouteResult = 0;
                planRoute(homeCompanyInfo, null);
            } else {
                if (type == 1) {
                    return havaNoHomeAddress();
                } else {
                    return haveNoCompanyAddress();
                }
            }
        } else {
            mKeyword = dest;
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, dest);
            setEndParam(bundle);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 获取目的地类型.
     *
     * @param dest 类型.
     * @return 1:家  2：公司  0:普通点.
     */
    private int getDestType(final String dest) {
        final int type = switch (dest) {
            case IVrBridgeConstant.DestType.HOME -> 1;
            case IVrBridgeConstant.DestType.COMPANY -> 2;
            default -> 0;
        };
        return type;
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
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(0);
            if (null != poiInfoEntity && poiInfoEntity.isIsLocres()) {
                //当前Feature，行政区划直接发起路线规划
                mPlanRouteResult = 0;
                ThreadManager.getInstance().asyncDelay(new Runnable() {
                    @Override
                    public void run() {
                        planRoute(poiInfoEntity, null);
                    }
                }, 300, TimeUnit.MILLISECONDS);
            } else {
                //关键字搜结果只有一个，需要poi详情搜结果返回后再发起算路
                mWaitPoiSearch = true;
            }
        } else {
            //搜索结果为多个，回调给语音，列表多轮选择
            responseSearchWithResult();
        }
    }

    /**
     * 回去搜索结果为行政区域，范围过大.
     */
    private void responsePoiHuge() {
        if (null != mPoiCallback) {
            final CallResponse poiDetailResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.DETAIL_SEARCH_NO_RESULT);
            poiDetailResponse.setSubCallResult(NaviSubCallResult.RESP_POI_SEARCH_KEYWORD_HUGE);
            poiDetailResponse.setNeedPlayMessage(false);
            mPoiCallback.onResponse(poiDetailResponse);
        }
    }

    /**
     * 搜结果唯一时触发poi详情搜后的处理.
     *
     * @param poiInfo PoiInfoEntity，poi详情信息.
     */
    private void dealPoiDetailResult(final PoiInfoEntity poiInfo) {
        Logger.w(IVrBridgeConstant.TAG, "process poiDetailSearch", mSearchType);
        if (null == poiInfo) {
            Logger.w(IVrBridgeConstant.TAG, "poiDetailSearchResult is empty");
            if (null != mPoiCallback) {
                final CallResponse poiDetailResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.DETAIL_SEARCH_NO_RESULT);
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
                mPlanRouteResult = 0;
                saveAndNaviToHomeCompany(poiInfo);
                break;
            case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                addCommonFavorite(poiInfo, false);
                break;
            default:
                mPlanRouteResult = 0;
                planRoute(poiInfo, null);
                break;
        }
    }

    /**
     * 根据路线结果数目回复提示信息.
     */
    public void playRouteResult() {
        if (mPlanRouteResult < 0) {
            return;
        }

        final CallResponse routeResponse = CallResponse.createSuccessResponse();
        routeResponse.setSubCallResult(NaviSubCallResult.RESP_MULTI_POI_SEARCH_SUCCESS);
        switch (mPlanRouteResult) {
            case 0:
            case 2:
                //使用PoiCallback回调或多目的地
                if (null != mPoiCallback) {
                    mPoiCallback.onResponse(routeResponse);
                }
                break;
            case 1:
                //使用RespCallback回调
                if (null != mRespCallback) {
                    mRespCallback.onResponse(routeResponse);
                }
                break;
            default:
                break;
        }
        mPlanRouteResult = -1;
    }


    /**
     * 创建未设置家地址的回复.
     *
     * @return CallResponse, 语音指令回复.
     */
    private CallResponse havaNoHomeAddress() {
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ADD_HOME_WITH_SAY);
    }

    /**
     * 创建未设置公司地址的回复.
     *
     * @return CallResponse, 语音指令回复.
     */
    private CallResponse haveNoCompanyAddress() {
        return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ADD_COMPANY_WITH_SAY);
    }

    /**
     * 将搜索结果回调给语音.
     */
    private void responseSearchWithResult() {
        if (null != mPoiCallback) {
            try {
                final List<PoiBean> poiBeanList = VoiceConvertUtil.convertSearchResult(mSearchResultList);
                final int size = poiBeanList.size();
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "responseToVoice, totalSearchSize: ", size);
                }
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
            final CallResponse response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_RESULT_TRY_OTHER);
            response.setNeedPlayMessage(true);
            mPoiCallback.onResponse(response);
        }
    }

    /**
     * 带关键字回复搜索结果为空.
     */
    private void responseSearchEmptyWithKeyword() {
        if (null != mPoiCallback) {
            final CallResponse response = CallResponse.createFailResponse(
                    IVrBridgeConstant.ResponseString.SEARCH + mKeyword + IVrBridgeConstant.ResponseString.EMPTY_RESULT_TRY_OTHER);
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

        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        final boolean inNavi = NaviStatus.NaviStatusType.NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus);
        final boolean inRoute = NaviStatus.NaviStatusType.ROUTING.equals(curStatus)
                || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus);
        if (inNavi || inRoute) {
            if (inRoute) {
                mPlanRouteResult = 1;
            } else {
                mPlanRouteResult = -1;
            }
            RoutePackage.getInstance().requestRouteFromSpeech(requestParam);
        } else {
            //打开算路界面
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ROUTING);
            bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST, requestParam);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        }
    }

    /**
     * 判断当前是否处理引导态.
     *
     * @return true:引导态   false:非引导态.
     */
    private boolean judgeNaviStatusForRouting() {
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        return NaviStatus.NaviStatusType.NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.ROUTING.equals(curStatus)
                || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus);
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.MULTI_DEST_EMPTY);
        }

        mNormalDestList.clear();
        mGenericsDestList.clear();
        mMultiplePoiArray.clear();
        mProcessDestIndex = -1;

        boolean containHome = false;
        boolean containCompany = false;
        //处理途径点
        final int size = passByPoi.size();
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "disposeMultipleDest: passByPoiSize = ", size);
        }
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
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "process ", type);
                }
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
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "process ", destType);
            }
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
            }
        }
        if (containCompany) {
            final PoiInfoEntity companyInfo = getHomeCompanyPoiInfo(2);
            if (null == companyInfo) {
                Logger.w(IVrBridgeConstant.TAG, "MultipleDest contain company but info is empty");
                return haveNoCompanyAddress();
            }
        }

        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "multipleDest normal size: ", mNormalDestList.size(), ", generics size: ", mGenericsDestList.size());
        }
        dealNextMultipleDest(null);
        if(Logger.openLog) {
            Logger.i(IVrBridgeConstant.TAG, "multiple return success response");
        }
        final CallResponse response = CallResponse.createSuccessResponse(IVrBridgeConstant.ResponseString.PROCESSING_MULTI_DEST);
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
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "multiple dest get generics ", mKeyword, " search result");
            }
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
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "genericsDestIndex: ", mProcessDestIndex);
            }
            final SingleDestInfo genericsDest = mGenericsDestList.get(mProcessDestIndex);
            mKeyword = genericsDest.getDestName();
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "genericsDestName: ", mKeyword);
            }
            mGenericsDestList.remove(mProcessDestIndex);
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        } else if (null != mNormalDestList && mNormalDestList.size() > 0) {
            //静默搜索普通poi
            mProcessDestIndex = mNormalDestList.keyAt(0);
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "normalDestIndex: ", mProcessDestIndex);
            }
            final SingleDestInfo normalDest = mNormalDestList.get(mProcessDestIndex);
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "normalDestTYPE: ", normalDest.getDestType());
            }
            mNormalDestList.remove(mProcessDestIndex);
            if (IVrBridgeConstant.DestType.HOME.equals(normalDest.getDestType())) {
                dealNextMultipleDest(getHomeCompanyPoiInfo(1));
            } else if (IVrBridgeConstant.DestType.COMPANY.equals(normalDest.getDestType())) {
                dealNextMultipleDest(getHomeCompanyPoiInfo(2));
            } else {
                mKeyword = normalDest.getDestName();
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "normalDestName: ", mKeyword);
                }
                mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, mKeyword);
            }
        } else if (null != mMultiplePoiArray && mMultiplePoiArray.size() > 0) {
            //全部获取完毕，开始路线规划
            int size = mMultiplePoiArray.size();
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "totalMultiple poi size: ", size);
            }
            final PoiInfoEntity endPoi = mMultiplePoiArray.valueAt(size - 1);
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "poiName: ", endPoi.getName(), ", address: ", endPoi.getAddress(), ", pid: ", endPoi.getPid());
            }
            mMultiplePoiArray.removeAt(size - 1);

            size = mMultiplePoiArray.size();
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "afterRemoveEnd size: ", size);
            }
            final List<PoiInfoEntity> viaList = new ArrayList<>();
            for (int i = 0; i < size; i++) {
                viaList.add(mMultiplePoiArray.valueAt(i));
            }
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "multipleDest viaSize:", size);
            }
            //规划路线
            mPlanRouteResult = 2;
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

        final int searchSize = null != mSearchResultList ? mSearchResultList.size() : 0;
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "selectPoiIndex: ", index, "resultSize", searchSize);
        }
        if (searchSize == 0) {
            responsePreviousSearchEmpty(respCallback);
            return;
        }

        if (searchSize > index) {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(index);
            if (null != poiInfoEntity) {
                disposeSelectedPoi(poiInfoEntity, respCallback);
            }
        } else {
            if (null != respCallback) {
                final CallResponse callResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.OUT_OF_CHOICE_RANGE);
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
            final CallResponse callResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.LAST_SEARCH_RESULT_EMPTY);
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
            final CallResponse response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.SESSION_ID_NOT_MATCH);
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
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "selectPoi with rule: ", rule);
        }
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
                respCallback.onResponse(CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_COMMENT));
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
        } else {
            respCallback.onResponse(CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_PROPER_DEST));
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
        } else {
            respCallback.onResponse(CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_PROPER_DEST));
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
        } else {
            respCallback.onResponse(CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_PROPER_DEST));
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
        } else {
            respCallback.onResponse(CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_PROPER_DEST));
        }
    }

    /**
     * 处理最后选中的Poi.
     *
     * @param poiInfo      PoiInfoEntity，选定的Poi信息
     * @param respCallback 语音响应回调.
     */
    private void disposeSelectedPoi(final PoiInfoEntity poiInfo, final RespCallback respCallback) {
        if (Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "processSelectPoi, searchType", mSearchType);
        }
        mSearchResultList.clear();
        mRespCallback = respCallback;
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
                mPlanRouteResult = 1;
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfo);
                break;
            case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                addCommonFavorite(poiInfo, false);
                sendClosePage();
                break;
            case IVrBridgeConstant.VoiceSearchType.NAVI_TO_HOME_COMPANY:
                mPlanRouteResult = 1;
                saveAndNaviToHomeCompany(poiInfo);
                break;
            default:
                mPlanRouteResult = 1;
                planRoute(poiInfo, null);
                break;
        }
    }


    /**
     * 发送指令，关闭当前HMI界面，如设置家/公司、途径点选择等.
     */
    public void sendClosePage() {
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
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "conditionSearch map is empty");
            }
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.SEARCH_CONDITION_EMPTY);
        }

        mSortValue = null;
        mSearchCondition = new VoiceSearchConditions();
        mSearchCondition.parseConditionMap(conditionMap);
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "afterParse, conditions: ", mSearchCondition);
        }
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
            //先处理中心点
            final int destType = getDestType(center);
            if (destType == 1 || destType == 2) {
                //中心点是家和公司
                final PoiInfoEntity homeCompanyInfo = getHomeCompanyPoiInfo(destType);
                if (null != homeCompanyInfo && null != homeCompanyInfo.getPoint()) {
                    dealAfterConditionCenter(homeCompanyInfo.getPoint());
                } else if (destType == 1) {
                    return havaNoHomeAddress();
                } else {
                    return haveNoCompanyAddress();
                }
            } else {
                mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, center);
            }
        } else if (VoiceConvertUtil.isNumber(distance)) {
            //当前位置的周边搜，指定半径
            final LocInfoBean curLocation = PositionPackage.getInstance().getLastCarLocation();
            if (null != curLocation) {
                final GeoPoint geoPoint = new GeoPoint(curLocation.getLongitude(), curLocation.getLatitude());
                dealAfterConditionCenter(geoPoint);
            } else {
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_GET_POS_INFO);
            }
        } else {
            //执行关键字搜索
            mSearchType = IVrBridgeConstant.VoiceSearchType.CONDITION_IN_PAGE;
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
            setEndParam(bundle);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
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
        if (!success || null == centerInfo || null == centerInfo.getPoint()) {
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

        mCenterPoint = geoPoint;
        mSearchType = IVrBridgeConstant.VoiceSearchType.CONDITION_IN_PAGE;
        aroundSearchByPoint(mCenterPoint);
    }

    private void aroundSearchByPoint(final GeoPoint geoPoint) {
        final String distance = mSearchCondition.getDistance();
        int radius = VoiceConvertUtil.getIntValue(distance);
        radius = radius > 0 ? radius : 5000;
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.AROUND_SEARCH);
        bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
        bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.AROUND_POINT, geoPoint);
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.AROUND_RADIUS, radius);
        setEndParam(bundle);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
    }

    /**
     * 当前已处于选路或引导太，但语音输入的是导航去xxx，需要搜索结果展示"去这里"
     *
     * @param bundle Bundle，fragment接收数据.
     */
    private void setEndParam(final Bundle bundle) {
        if (null == bundle) {
            return;
        }

        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        if (NaviStatus.NaviStatusType.NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)
                || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus)) {
            bundle.putBoolean(IVrBridgeConstant.VoiceIntentParams.IS_END, true);
        }
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
    public CallResponse handlePassBy(final String sessionId, final String passBy, final String poiType, final PoiCallback poiCallback) {
        final int type = getDestType(passBy);
        if (type == 1 || type == 2) {
            final PoiInfoEntity homeCompanyInfo = getHomeCompanyPoiInfo(type);
            if (null != homeCompanyInfo) {
                RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, homeCompanyInfo);
                return CallResponse.createSuccessResponse();
            } else {
                CallResponse callResponse;
                callResponse = CallResponse.createFailResponse("");
                if (type == 1) {
                    callResponse.setSubCallResult(NaviSubCallResult.TRAFFIC_QUERY_NO_HOME);
                } else {
                    callResponse.setSubCallResult(NaviSubCallResult.TRAFFIC_QUERY_NO_COMPANY);
                }
                poiCallback.onResponse(callResponse);
                return CallResponse.createSuccessResponse();
            }
        }

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
        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理沿途搜结果，一个或多个结果都需要用户确认添加途径点.
     */
    private void dealAlongWaySearchResult() {
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            if (mAlongToAround) {
                //已经触发沿途搜转周边
                mAlongToAround = false;
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "AlongSearch turnAround: ", mKeyword, ", result is empty");
                }
                responseSearchEmpty();
            } else {
                mAlongToAround = true;
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "AlongSearch ", mKeyword, "result is empty, turnAroundSearch");
                }
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
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_SETTING_TYPE);
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
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_GET_LOCATION);
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
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_SETTING_TYPE);
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
        if (null == poiInfo || null == poiInfo.getPoint()) {
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
        if (null == poiInfo || null == poiInfo.getPoint()) {
            return;
        }

        final FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(type);
        poiInfo.setFavoriteInfo(favoriteInfo);
        if (ConvertUtils.isEmpty(poiInfo.getPid())) {
            poiInfo.setPid(poiInfo.getPoint().getLon() + "_" + poiInfo.getPoint().getLat());
        }
        BehaviorPackage.getInstance().addFavorite(poiInfo, type);

        if (mRespCallback != null) {
            mRespCallback.onResponse(CallResponse.createSuccessResponse());
        }
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
                mPlanRouteResult = 0;
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
                final CallResponse locationResponse = CallResponse.createSuccessResponse(
                        IVrBridgeConstant.ResponseString.CURRENT_LOCATE + address + IVrBridgeConstant.ResponseString.AROUND);
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
        final CallResponse favoriteResponse;
        if (null != poiInfo && null != poiInfo.getPoint()) {
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "current name: ", poiInfo.getName()
                        + ", address: " + poiInfo.getAddress()
                        + ",lon: " + poiInfo.getPoint().getLon()
                        + ", lat: " + poiInfo.getPoint().getLat());
            }
            final boolean alreadySaved = BehaviorPackage.getInstance().alreadySavedPoi(poiInfo);
            if (alreadySaved) {
                favoriteResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.ALREADY_SAVED);
            } else {
                favoriteResponse = CallResponse.createSuccessResponse(
                        IVrBridgeConstant.ResponseString.ADD_FAVORITE + poiInfo.getName());
                final FavoriteInfo favoriteInfo = new FavoriteInfo();
                favoriteInfo.setCommonName(0);
                if (ConvertUtils.isEmpty(poiInfo.getPid())) {
                    poiInfo.setPid(poiInfo.getPoint().getLon() + "_" + poiInfo.getPoint().getLat());
                }
                poiInfo.setFavoriteInfo(favoriteInfo);
                BehaviorPackage.getInstance().addFavorite(poiInfo, 0);
            }
        } else {
            favoriteResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NO_SUCH_INFO);
        }

        favoriteResponse.setNeedPlayMessage(true);
        if (geoSearch && null != mRespCallback) {
            mRespCallback.onResponse(favoriteResponse);
        } else if (null != mPoiCallback) {
            mPoiCallback.onResponse(favoriteResponse);
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_ADD_EMPTY_POI);
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
                final CallResponse etaResponse = CallResponse.createFailResponse(
                        IVrBridgeConstant.ResponseString.CANT_FIND + mKeyword + IVrBridgeConstant.ResponseString.TRY_OTHER);
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
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "homeCompanyEta: ", homeCompanyEta);
                }
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
            final CallResponse noneMsgResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_SUCH_INFO);
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
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NET_TRASH);
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
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.CANT_GET_POS_INFO);
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
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "twoPoiEtaInfo: ", twoPointEta);
                }
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
    public CallResponse sortPoi(final String sessionId, final String type, final String rule, final RespCallback respCallback) {
        if(Logger.openLog) {
            Logger.d(IVrBridgeConstant.TAG, "poiSort rule: ", rule);
        }
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "poiSort searchResult is empty");
            }
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.LAST_SEARCH_RESULT_EMPTY);
        }
        if(!mSupportSortPoi) {
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "sort info is empty");
            }
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.UN_SUPPORT_SORT);
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
            case IVrBridgeConstant.PoiSortType.RECOMMEND:
                //推荐排序
                mSortValue = IVrBridgeConstant.PoiSortValue.PRIORITY_RECOMMEND;
                break;
            default:
                if(Logger.openLog) {
                    Logger.d(IVrBridgeConstant.TAG, "unKnown sortType");
                }
                break;
        }

        if (TextUtils.isEmpty(mSortValue)) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_CURRENT_SORT);
        } else {
            mSearchType = IVrBridgeConstant.VoiceSearchType.POI_SORT;
            mRespCallback = respCallback;
            if (IVrBridgeConstant.PoiSortValue.PRIORITY_RECOMMEND.equals(mSortValue)) {
                //综合排序，使用默认搜索第一页的结果
                if (mCurrentPage < 1) {
                    mCurrentPage = 1;
                }
                if (null != mCenterPoint && null != mSearchCondition) {
                    aroundSearchByPoint(mCenterPoint);
                } else {
                    final Bundle bundle = new Bundle();
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
                    bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
                    bundle.putInt(IVrBridgeConstant.VoiceIntentParams.TARGET_PAGE, mCurrentPage);
                    setEndParam(bundle);
                    MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
                }
            } else {
                if (null != mCenterPoint) {
                    SearchPackage.getInstance().voiceSortPoi(MapType.MAIN_SCREEN_MAIN_MAP, mSortValue, mCenterPoint);
                } else {
                    SearchPackage.getInstance().voiceSortPoi(MapType.MAIN_SCREEN_MAIN_MAP, mSortValue);
                }
            }
            return CallResponse.createSuccessResponse();
        }
    }

    /**
     * 回复不支持当前筛选项.
     *
     * @param respCallback RespCallback,语音值结果回调.
     */
    private void responseUnSupportSortRule(final RespCallback respCallback) {
        if (null != respCallback) {
            final CallResponse callResponse = CallResponse.createNotSupportResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_CURRENT_SORT);
            callResponse.setNeedPlayMessage(true);
            respCallback.onResponse(callResponse);
        }
    }

    /**
     * 处理排序筛选搜索结果.
     */
    private void dealSortResult() {
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            if(Logger.openLog) {
                Logger.d(IVrBridgeConstant.TAG, "poiSort type: ", mSortValue, " search result empty");
            }
            if (null != mRespCallback) {
                final CallResponse sortResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.SORT_RESULT_EMPTY);
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
                if(Logger.openLog) {
                    Logger.i(IVrBridgeConstant.TAG, "unSupport sortValue: ", mSortValue);
                }
                break;
        }

        if (appendHint) {
            String distance = firstPoi.getDistance();
            if (TextUtils.isEmpty(distance) || VoiceConvertUtil.ZERO_DIST.equals(distance)) {
                distance = VoiceConvertUtil.formatDistance(firstPoi.getPoint());
            }
            builder.append(name).append("距您").append(distance).append("，去这里可以吗");
        }

        if (null != mRespCallback) {
            final String hintMsg = builder.toString();
            final CallResponse sortResultResponse;
            if (TextUtils.isEmpty(hintMsg)) {
                sortResultResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NOT_SUPPORT_SORT_TYPE);
            } else {
                sortResultResponse = CallResponse.createSuccessResponse(hintMsg);
            }
            sortResultResponse.setNeedPlayMessage(true);
            mRespCallback.onResponse(sortResultResponse);
        }
    }


    /**
     * 处理语音翻页.
     *
     * @param sessionId 多轮对话一致.
     *
     * @param type  direction：方位，比如上一页、下一页
     *              index：正向位置，比如第一页
     *              index_reverse：反向位置，比如倒数第一页，最后一页
     *
     * @param typeValue  direction = UP:上一页；DOWN：下一页
     *                   index、index_reverse >= 1，具体页码值
     *
     * @param respCallback 执行结果异步回复.
     *
     * @return CallResponse 此指令直接执行结果.
     */
    public CallResponse handlePoiPage(final String sessionId, final String type, final String typeValue, final RespCallback respCallback) {
        if (!mListPageOpened) {
            //搜索结果页没有打开
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.HAVE_NO_POI_PAGE);
        }

        final CallResponse callResponse;
        switch (type) {
            case IVrBridgeConstant.PoiPageType.DIRECTION:
                if (IVrBridgeConstant.PageTypeValue.DOWN.equals(typeValue)) {
                    //下一页
                    if (mCurrentPage == mMaxPage) {
                        //当前已是最后一页
                        callResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ALREADY_LAST_PAGE);
                    } else {
                        callResponse = toTargetPage(sessionId, mCurrentPage + 1, respCallback);
                    }
                } else {
                    //上一页
                    if (mCurrentPage == 1) {
                        callResponse = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ALREADY_FIRST_PAGE);
                    } else {
                        callResponse = toTargetPage(sessionId, mCurrentPage - 1, respCallback);
                    }
                }
                break;
            case IVrBridgeConstant.PoiPageType.INDEX:
                //正向第几页
                final int target = StringUtils.stringToInt(typeValue);
                callResponse = toSearchPage(sessionId, target, respCallback);
                break;
            case IVrBridgeConstant.PoiPageType.REVERSE_INDEX:
                //反向第几页
                final int param = StringUtils.stringToInt(typeValue);
                if (param < 1) {
                    return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.PAGE_ERROR_PARAM);
                }
                final int reversePage = mMaxPage + 1 - param;
                callResponse = toSearchPage(sessionId, reversePage, respCallback);
                break;
            default:
                return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.PAGE_ERROR_PARAM);
        }

        return callResponse;
    }

    private CallResponse toSearchPage(final String sessionId, final int target, final RespCallback respCallback) {
        if (target < 1 || target > mMaxPage) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.OUT_OF_CHOICE_RANGE);
        } else if (mCurrentPage == target) {
            return CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.ALREADY_CURRENT_PAGE);
        } else {
            return toTargetPage(sessionId, target, respCallback);
        }
    }

    private CallResponse toTargetPage(final String sessionId, final int target, final RespCallback respCallback) {
        mTurnCallback = respCallback;
        mInTurnRound = true;
        mSessionId = sessionId;
        final Bundle bundle = new Bundle();
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE,
                IVrBridgeConstant.VoiceIntentPage.TURN_TARGET);
        bundle.putInt(IVrBridgeConstant.VoiceIntentParams.TARGET_PAGE, target);
        MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        return CallResponse.createSuccessResponse();
    }

    //处理翻页后的搜索结果
    private void dealTurnPageResult(final boolean success) {
        mInTurnRound = false;
        if (success) {
            final List<PoiBean> poiBeanList = VoiceConvertUtil.convertSearchResult(mSearchResultList);
            final int size = poiBeanList.size();
            if (Logger.openLog) {
                Logger.i(IVrBridgeConstant.TAG, "turnPage resultSize:", size);
            }
            if (null != mPoiCallback) {
                mPoiCallback.onPoiSearch(mSessionId, poiBeanList, size);
            }
        } else if (null != mTurnCallback) {
            final CallResponse response = CallResponse.createFailResponse(IVrBridgeConstant.ResponseString.NO_RESULT_TRY_OTHER);
            response.setNeedPlayMessage(true);
            mTurnCallback.onResponse(response);
        }
    }


}
