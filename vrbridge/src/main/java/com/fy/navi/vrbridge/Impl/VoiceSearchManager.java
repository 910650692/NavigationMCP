package com.fy.navi.vrbridge.impl;

import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.util.SparseArray;

import com.baidu.oneos.protocol.bean.ArrivalBean;
import com.baidu.oneos.protocol.bean.CallResponse;
import com.baidu.oneos.protocol.bean.PoiBean;
import com.baidu.oneos.protocol.callback.PoiCallback;
import com.baidu.oneos.protocol.callback.RespCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.greendao.favorite.Favorite;
import com.fy.navi.service.greendao.favorite.FavoriteManager;
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
 * @author tssh.
 * @version $Revision.1.0.0$
 */
public final class VoiceSearchManager {

    private String mSessionId; //多轮对话标识
    private ArrivalBean mDestInfo; //搜索/导航意图条件集合
    private PoiCallback mPoiCallback; //给语音的搜索结果回调
    private int mSearchType; //语音内部使用，当前语音搜索/导航指令类型，用于多轮
    private int mSearchTaskId; //搜索指令返回的task值
    private String mKeyword;
    private String mRouteType;
    private List<PoiInfoEntity> mSearchResultList; //多轮选择保存的搜索结果

    private List<String> mGenericsList; //DestType为泛型的集合，用来判断途径点和目的地是不是泛型
    private SparseArray<SingleDestInfo> mNormalDestList; //多目的地普通点集合
    private SparseArray<SingleDestInfo> mGenericsDestList; //多目的地泛型点集合
    private int mProcessDestIndex = -1; //当前正在处理的多目的地下标
    private SparseArray<PoiInfoEntity> mMultiplePoiArray; //多目的地途径点和目的地集合

    private String mPoiType; //地址类型，用于设置家、公司、普通点

    private VoiceSearchConditions mSearchCondition; //深度-筛选搜索项
    private String mSortValue;

    private boolean mAlongToAround = false; //是否为沿途搜转周边搜，在沿途搜结果为空时触发

    //语音结果回调
    private RespCallback mRespCallback;



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

        mNormalDestList.clear();
        mGenericsDestList.clear();
        mMultiplePoiArray.clear();
        mProcessDestIndex = -1;
    }

    private final SearchResultCallback mSearchCallback = new SearchResultCallback() {

        @Override
        public void onSearchResult(final int taskId, final  int errorCode,final  String message,final  SearchResultEntity searchResultEntity) {
            String keyword = "";
            if (null != searchResultEntity) {
                keyword = searchResultEntity.getKeyword();
            }
            if (!(mSearchTaskId == taskId || Objects.equals(mKeyword, keyword))) {
                return;
            }

            mSearchTaskId = -1;
            final boolean searchSuccess;
            mSearchResultList.clear();
            if (null == searchResultEntity || null == searchResultEntity.getPoiList() || searchResultEntity.getPoiList().isEmpty()) {
                Log.e(IVrBridgeConstant.TAG, "searchResult is empty");
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
                    //设置公司或家地址 1.逆地址搜索回调当前位置  2.关键字搜索结果
                    dealHomeCompanyResult(searchSuccess);
                    break;
                case IVrBridgeConstant.VoiceSearchType.SHOW_POI_DETAIL:
                    //查询当前位置
                    mKeyword = null;
                    if (searchSuccess) {
                        final PoiInfoEntity poiInfoEntity = mSearchResultList.get(0);
                        Log.d(IVrBridgeConstant.TAG, "showPoiInfo: " + poiInfoEntity.getAddress());
                        showPoiDetail(poiInfoEntity);
                    } else if (null != mRespCallback) {
                        mRespCallback.onResponse(CallResponse.createFailResponse("不好意思，我定位不到你在哪里"));
                    }
                    break;
                case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                    //收藏当前位置
                    if (searchSuccess) {
                        final PoiInfoEntity poiInfoEntity = mSearchResultList.get(0);
                        addCommonFavorite(poiInfoEntity);
                    } else if (null != mRespCallback) {
                        mRespCallback.onResponse(CallResponse.createFailResponse("未查询到定位信息，无法收藏"));
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
                default:
                    break;
            }
        }

        @Override
        public void onSilentSearchResult(final int taskId, final  int errorCode, final String message, final SearchResultEntity searchResultEntity) {
            //静默搜索结果回调，多筛选条件和多目的地搜
            if (mSearchTaskId != taskId) {
                return;
            }

            mSearchTaskId = -1;
            boolean searchSuccess = true;
            if (null == searchResultEntity || null == searchResultEntity.getPoiList() || searchResultEntity.getPoiList().isEmpty()) {
                Log.e(IVrBridgeConstant.TAG, "searchResult is empty");
                searchSuccess = false;
            }
            PoiInfoEntity firstSearchResult = null;
            mSearchResultList.clear();
            if (searchSuccess) {
                firstSearchResult = searchResultEntity.getPoiList().get(0);
                mSearchResultList.addAll(searchResultEntity.getPoiList());
            }

            switch (mSearchType) {
                case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                    //多目的地普通Poi点
                    dealMultipleDestResult(searchSuccess, false);
                    break;
                case IVrBridgeConstant.VoiceSearchType.WITH_CONDITION:
                    //先搜索中心点再打开周边搜界面
                    dealConditionCenterResult(searchSuccess, firstSearchResult);
                    break;
                case IVrBridgeConstant.VoiceSearchType.ADD_FAVORITE:
                    //搜索Poi信息，添加收藏
                    dealAddFavoriteResult(searchSuccess);
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
     *
     * @return CallResponse,语音直接结果回调.
     */
    public CallResponse handleCommonSearch(final  String sessionId, final ArrivalBean arrivalBean, final PoiCallback poiCallback) {
        if (null == poiCallback) {
            return CallResponse.createFailResponse("空的搜索结果回调");
        }
        mPoiCallback = poiCallback;
        if (null == arrivalBean || TextUtils.isEmpty(arrivalBean.getDest())) {
            Log.e(IVrBridgeConstant.TAG, "voiceSearchParams is null");
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
        Log.d(IVrBridgeConstant.TAG, " keywordSearch voiceInnerType: " + mSearchType);
        switch (mSearchType) {
            case IVrBridgeConstant.VoiceSearchType.ONLY_KEYWORD:
            case IVrBridgeConstant.VoiceSearchType.WITH_PREFER:
                //单目的地或带偏好
                return disposeSingleDest(dest);
            case IVrBridgeConstant.VoiceSearchType.WITH_PASS_BY:
                //多目的地
                return disposeMultipleDest();
            case IVrBridgeConstant.VoiceSearchType.WITH_CONDITION:
                //深度Poi多轮
            return disposeConditionSearch();
            default:
                Log.w(IVrBridgeConstant.TAG, "unHandle searchType: " + mSearchType);
                return CallResponse.createNotSupportResponse("不支持的搜索/导航意图");
        }
    }

    /**
     * 处理单目的地（带偏好）的搜索/导航意图.
     *
     * @param dest String，目的地类型或关键字.
     *
     * @return CallResponse,语音执行结果.
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
                planRoute(homeCompanyInfo, null);
            } else {
                if (type == 1) {
                    return havaNoHomeAddress();
                } else {
                    return havaNoCompanyAddress();
                }
            }
        } else {
            jumpToSearchPage(dest);
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 获取保存的家或公司信息.
     * @param type 1-家  2-公司
     * @return PoiInfoEntity.
     */
    private PoiInfoEntity getHomeCompanyPoiInfo(final int type) {
        final List<Favorite> favoriteList = FavoriteManager.getInstance().getValueByCommonName(type);
        if (null == favoriteList || favoriteList.isEmpty()) {
            return null;
        }
        final Favorite favoriteItem = favoriteList.get(0);
        return VoiceConvertUtil.getPoiInfoByFavorite(favoriteItem);
    }

    /**
     * 根据输入的关键字跳转到搜索结果页.
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
            //关键字搜结果只有一个，直接选择结果作为目的地发起算路
            final PoiInfoEntity endPoi = mSearchResultList.get(0);
            planRoute(endPoi, null);
        } else {
            //搜索结果为多个，回调给语音，列表多轮选择
            responseSearchWithResult();
        }
    }

    /**
     * 创建未设置家地址的回复.
     *
     * @return CallResponse,语音指令回复.
     */
    private CallResponse havaNoHomeAddress() {
        return CallResponse.createFailResponse("未找到家的地址，先去添加吧，试试说：设置家的地址");
    }

    /**
     * 创建未设置公司地址的回复.
     *
     * @return CallResponse,语音指令回复.
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
                Log.d(IVrBridgeConstant.TAG, "response poiBeanListSize : " + size);
                mPoiCallback.onPoiSearch(mSessionId, poiBeanList, size);
            } catch (NullPointerException npe) {
                Log.w(IVrBridgeConstant.TAG, "responseSearchPoi error: " + npe.getMessage());
            }
        }
    }

    /**
     * 回复空搜索结果.
     */
    private void responseSearchEmpty() {
        if (null != mPoiCallback) {
            mPoiCallback.onResponse(CallResponse.createFailResponse("未找到相关结果，试试别的吧"));
        }
    }

    /**
     * 带关键字回复搜索结果为空.
     */
    private void responseSearchEmptyWithKeyword() {
        if (null != mPoiCallback) {
            mPoiCallback.onResponse(CallResponse.createFailResponse("搜索" + mKeyword + "结果为空，试试别的吧"));
        }
    }

    /**
     * 根据选中目的地发起算路.
     *
     * @param endPoi 目的地Poi信息
     * @param viaList 途径点列表.
     */
    private void planRoute(final PoiInfoEntity endPoi, final List<PoiInfoEntity> viaList) {
        final RouteSpeechRequestParam requestParam = new RouteSpeechRequestParam();
        requestParam.setMEndPoiInfoEntity(endPoi);
        requestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
        if (null != mRouteType) {
            final RoutePreferenceID currPrefer = VoiceConvertUtil.convertToAMapPrefer(mRouteType);
            requestParam.setMPreferenceID(currPrefer);
            mRouteType = null;
        } else {
            requestParam.setMPreferenceID(SettingPackage.getInstance().getRoutePreference());
        }
        if (!(null == viaList || viaList.isEmpty())) {
            requestParam.setMViaPoiInfoEntityList(viaList);
        }
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
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
     * 处理多目的地导航.
     *
     * @return CallResponse,语音指令回复.
     */
    private CallResponse disposeMultipleDest() {
        //解析途径点和目的地信息
        final SparseArray<PoiBean> passByPoi = mDestInfo.getPassbyPoi();
        if (null == passByPoi || passByPoi.size() == 0) {
            Log.w(IVrBridgeConstant.TAG, "passBy is empty");
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
        Log.d(IVrBridgeConstant.TAG, "disposeMultipleDest: passByPoiSize = " + size);
        for (int i = 0; i < size; i++) {
            final PoiBean poiBean = passByPoi.valueAt(i);
            final String name = poiBean.getName();
            final String type = poiBean.getType();
            final SingleDestInfo singleDestInfo = new SingleDestInfo();
            singleDestInfo.setDestName(name);
            singleDestInfo.setDestType(type);

            if(IVrBridgeConstant.DestType.HOME.equals(name)) {
                containHome = true;
                mNormalDestList.put(i, singleDestInfo);
            } else if (IVrBridgeConstant.DestType.COMPANY.equals(name)) {
                containCompany = true;
                mNormalDestList.put(i, singleDestInfo);
            } else if (mGenericsList.contains(type)) {
                Log.d(IVrBridgeConstant.TAG, "process " + type);
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
        if(IVrBridgeConstant.DestType.HOME.equals(destName)) {
            containHome = true;
            mNormalDestList.put(size, singleDestInfo);
        } else if (IVrBridgeConstant.DestType.COMPANY.equals(destName)) {
            containCompany = true;
            mNormalDestList.put(size, singleDestInfo);
        } else if (mGenericsList.contains(destType)) {
            Log.d(IVrBridgeConstant.TAG, "process " + destType);
            mGenericsDestList.put(size, singleDestInfo);
        } else {
            mNormalDestList.put(size, singleDestInfo);
        }

        if (containHome) {
            final PoiInfoEntity homeInfo = getHomeCompanyPoiInfo(1);
            if (null == homeInfo) {
                Log.w(IVrBridgeConstant.TAG, "MultipleDest contain home but info is empty");
                return havaNoHomeAddress();
            }
        }
        if (containCompany) {
            final PoiInfoEntity companyInfo = getHomeCompanyPoiInfo(2);
            if (null == companyInfo) {
                Log.w(IVrBridgeConstant.TAG, "MultipleDest contain company but info is empty");
                return havaNoCompanyAddress();
            }
        }

        Log.d(IVrBridgeConstant.TAG, "multipleDest normal size: " + mNormalDestList.size() + ", generics size: " + mGenericsDestList.size());
        dealNextMultipleDest(null);
        return CallResponse.createSuccessResponse();
    }

    /**
     * 根据搜索结果执行多目的地下一步.
     *
     * @param success 是否搜索成功.
     * @param genericsPoi 是否为泛型搜索结果. true-将结果回调给语音供用户选择  false-默认选取第一个搜索结果.
     */
    private void dealMultipleDestResult(final boolean success, final boolean genericsPoi) {
        if (!success) {
            Log.e(IVrBridgeConstant.TAG, "silentSearch has not result");
            responseSearchEmptyWithKeyword();
            return;
        }

        if (genericsPoi) {
            Log.d(IVrBridgeConstant.TAG, "multiple dest get generics " + mKeyword + " search result");
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
            final SingleDestInfo genericsDest = mGenericsDestList.get(mProcessDestIndex);
            mGenericsDestList.remove(mProcessDestIndex);
            mKeyword = genericsDest.getDestName();
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        } else if (null != mNormalDestList && mNormalDestList.size() > 0) {
            //静默搜索普通poi
            mProcessDestIndex = mNormalDestList.keyAt(0);
            final SingleDestInfo normalDest = mNormalDestList.get(mProcessDestIndex);
            mNormalDestList.remove(mProcessDestIndex);
            mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, normalDest.getDestName());
        } else if (null != mMultiplePoiArray && mMultiplePoiArray.size() > 0) {
            //全部获取完毕，开始路线规划
            int size = mMultiplePoiArray.size();
            Log.d(IVrBridgeConstant.TAG, "totalMultiple poi size: " + size);
            final PoiInfoEntity endPoi = mMultiplePoiArray.valueAt(size - 1);
            Log.d(IVrBridgeConstant.TAG, "poiName: " + endPoi.getName() + ", address: " + endPoi.getAddress() + ", pid: " + endPoi.getPid());
            mMultiplePoiArray.removeAt(size - 1);

            size = mMultiplePoiArray.size();
            Log.d(IVrBridgeConstant.TAG, "afterRemoveEnd size: " + size);
            final List<PoiInfoEntity> viaList = new ArrayList<>();
            for (int i = 0; i < size; i++) {
                viaList.add(mMultiplePoiArray.valueAt(i));
            }
            Log.d(IVrBridgeConstant.TAG, "via size: " + size);
            //规划路线
            planRoute(endPoi, viaList);
        }
    }

    /**
     * Poi多轮根据index选定Poi点.
     *
     * @param sessionId 语音多轮唯一标识.
     * @param index 搜索结果下标.
     * @param respCallback RespCallback，语音响应回调.
     */
    public void handlePoiSelectIndex(final String sessionId, final int index, final RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "selectPoiIndex: " + index);
        if (null == mSessionId || mSessionId.isEmpty() || !mSessionId.equals(sessionId)) {
            mSearchResultList.clear();
            responseNotMatchId(respCallback);
            return;
        }
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
                respCallback.onResponse(CallResponse.createFailResponse("超出选择范围"));
            }
        }
    }

    /**
     * 响应找不到上一次搜索结果.
     *
     * @param respCallback RespCallback.
     */
    private void responsePreviousSearchEmpty(final RespCallback respCallback) {
        Log.e(IVrBridgeConstant.TAG, "last searchResult is empty or index overview");
        if (null != respCallback) {
            respCallback.onResponse(CallResponse.createFailResponse("上一轮搜索结果为空"));
        }
    }

    /**
     * 回复两轮对话标识不匹配.
     *
     * @param respCallback RespCallback，语音执行结果回调.
     */
    private void responseNotMatchId(final RespCallback respCallback) {
        Log.e(IVrBridgeConstant.TAG, "last sessionId is empty or not matching");
        if (null != respCallback) {
            respCallback.onResponse(CallResponse.createFailResponse("sessionId不匹配"));
        }
    }

    /**
     * 根据条件选择搜索结果列表某一项.
     *
     * @param sessionId 语音多轮唯一标识.
     * @param rule 1-最近的  2-评分最高的  3-价格最低  4-价格最高.
     * @param respCallback 语音传入的结果回调接口.
     */
    public void handlePoiSelectRule(final String sessionId, final int rule, final RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "selectPoi with rule: " + rule);
        if (null == mSessionId || mSessionId.isEmpty() || !mSessionId.equals(sessionId)) {
            responseNotMatchId(respCallback);
            return;
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
                Log.e(IVrBridgeConstant.TAG, "not handle rule: " + rule);
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
     * @param poiInfo PoiInfoEntity，选定的Poi信息
     * @param respCallback 语音响应回调.
     */
    private void disposeSelectedPoi(final PoiInfoEntity poiInfo, final RespCallback respCallback) {
        if (null != respCallback) {
            respCallback.onResponse(CallResponse.createSuccessResponse());
        }
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
                sendClosePage();
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
     * 带条件的目的地检索.
     *
     * @return CallResponse,语音指令回复.
     */
    private CallResponse disposeConditionSearch() {
        final Map<String, String> conditionMap = mDestInfo.getConditions();
        if (null == conditionMap || conditionMap.isEmpty()) {
            Log.d(IVrBridgeConstant.TAG, "conditionSearch map is empty");
            return CallResponse.createFailResponse("检索条件为空");
        }

        mSortValue = null;
        mSearchCondition = new VoiceSearchConditions();
        mSearchCondition.parseConditionMap(conditionMap);
        Log.d(IVrBridgeConstant.TAG, "afterParse, conditions: " + mSearchCondition);
        //等级，类似5A/4A/3A/2A/1A/5星/4星/3星/2星/1星，如果存在，和dest一起拼接成keyword
        final String level = mSearchCondition.getLevel();
        if (!TextUtils.isEmpty(level)) {
            mKeyword = level + mDestInfo.getDest();
        } else {
            mKeyword = mDestInfo.getDest();
        }

        final String distance = mSearchCondition.getDistance();
        final String price = mSearchCondition.getPrice();
        final String rate = mSearchCondition.getRate();
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

        final String center = mSearchCondition.getCenter();
        if (TextUtils.isEmpty(center) && !VoiceConvertUtil.isNumber(distance)) {
            //执行关键字搜索
            mSearchType = IVrBridgeConstant.VoiceSearchType.CONDITION_IN_PAGE;
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mKeyword);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        } else {
            //触发周边搜
            if (TextUtils.isEmpty(center)) {
                //没有中心点，当前位置的周边搜
                final LocInfoBean curLocation = PositionPackage.getInstance().getLastCarLocation();
                if (null != curLocation) {
                    final GeoPoint geoPoint = new GeoPoint(curLocation.getLongitude(), curLocation.getLatitude());
                    dealAfterConditionCenter(geoPoint);
                }
            } else {
                //先搜索中心点
                mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, center);
            }
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 处理多条件搜索中心点搜索结果.
     * @param success 是否搜索成功
     * @param centerInfo 搜索中心点Poi信息.
     */
    private void dealConditionCenterResult(final boolean success, final PoiInfoEntity centerInfo) {
        if (!success && null == centerInfo || null == centerInfo.getPoint()) {
            Log.e(IVrBridgeConstant.TAG, "searchAround center is empty");
            responseSearchEmpty();
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
     * @param sessionId 语音多轮唯一标识.
     * @param passBy 关键字.
     * @param poiType 沿途点类型.
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
                Log.d(IVrBridgeConstant.TAG, "AlongSearch turnAround: " + mKeyword + ", result is empty");
                responseSearchEmpty();
            } else {
                mAlongToAround = true;
                Log.d(IVrBridgeConstant.TAG, "AlongSearch " + mKeyword + "result is empty, turnAround");
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
     * @param sessionId String，多轮对话保持一致性
     * @param poiType String，HOME-家，COMPANY-公司
     * @param poi String，CURRENT_LOCATION-当前地址 or poi名称
     * @param poiCallback PoiCallback，搜索结果回调.
     *
     * @return CallResponse,语音指令回复.
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
                mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint);
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
            } else if (null != mPoiCallback) {
                return CallResponse.createNotSupportResponse("不支持的设置类型");
            }
        }

        return CallResponse.createSuccessResponse();
    }

    /**
     * 根据搜索结果设置家/公司地址.
     *
     * @param searchSuccess 是否搜索成功.
     */
    private void dealHomeCompanyResult(final boolean searchSuccess) {
        if (!searchSuccess) {
            Log.w(IVrBridgeConstant.TAG, "setHomeCompany, searchResult is empty");
            responseSearchEmpty();
            return;
        }

        final int size = mSearchResultList.size();
        Log.w(IVrBridgeConstant.TAG, "setHomeCompany searchResultSize: " + size);
        if (size == 1) {
            updateHomeCompany(mSearchResultList.get(0));
            sendClosePage();
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

        Log.w(IVrBridgeConstant.TAG, "setHomeCompany poiName: " + poiInfo.getName());
        final FavoriteInfo favoriteInfo = new FavoriteInfo();
        final int type;
        if (IVrBridgeConstant.DestType.HOME.equals(mPoiType)) {
            //家
            type = 1;
            if (null != mPoiCallback) {
                mPoiCallback.onResponse(CallResponse.createFailResponse("已设置家的地址"));
            }
        } else {
            //公司
            type = 2;
            if (null != mPoiCallback) {
                mPoiCallback.onResponse(CallResponse.createFailResponse("已设置公司的地址"));
            }
        }

        favoriteInfo.setCommonName(type);
        poiInfo.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavorite(poiInfo, type);
    }

    /**
     * 通过逆地理搜索查询当前位置详细信息.
     *
     * @param searchType 查询当前位置的目的
     * @param geoPoint 当前定位信息
     * @param respCallback 语音响应回调.
     */
    public void queryCurrentLocationDetail(final int searchType, final GeoPoint geoPoint, final RespCallback respCallback) {
        mSearchType = searchType;
        mRespCallback = respCallback;
        mSearchTaskId = SearchPackage.getInstance().geoSearch(geoPoint);
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
                mRespCallback.onResponse(CallResponse.createSuccessResponse("当前定位在" + address + "附近"));
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
     * @param poiInfo PoiInfoEntity，POI信息.
     */
    private void addCommonFavorite(final PoiInfoEntity poiInfo) {
        if (null != poiInfo) {
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createSuccessResponse("已为你收藏" + poiInfo.getName()));
            }
            final FavoriteInfo favoriteInfo = new FavoriteInfo();
            favoriteInfo.setCommonName(0);
            poiInfo.setFavoriteInfo(favoriteInfo);
            BehaviorPackage.getInstance().addFavorite(poiInfo, 0);
        }
    }

    /**
     * 搜索关键字信息.
     *
     * @param searchType 搜索目的.
     * @param keyword 关键字.
     * @param respCallback 语音响应回调.
     */
    public void searchPoiInfo(final int searchType, final String keyword, final RespCallback respCallback) {
        if (TextUtils.isEmpty(keyword)) {
            return;
        }
        mSearchType = searchType;
        mRespCallback = respCallback;
        mSearchTaskId = SearchPackage.getInstance().silentKeywordSearch(1, keyword);
    }

    /**
     * 根据搜索结果处理收藏流程.
     *
     * @param success 是否搜索成功.
     */
    private void dealAddFavoriteResult(final boolean success) {
        if (!success) {
            Log.w(IVrBridgeConstant.TAG, "addFavorite search " + mKeyword + " result is empty");
            responseSearchEmpty();
        } else {
            final PoiInfoEntity poiInfoEntity = mSearchResultList.get(0);
            addCommonFavorite(poiInfoEntity);
        }
    }

    /**
     * 根据搜索获取的结果计算与终点的ETA信息.
     *
     * @param searchType 语音搜索目的
     * @param poiType 终点类型, HOME-家  COMPANY-公司.
     * @param keyword 关键字.
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
    private void dealEtaResult(final boolean success, final PoiInfoEntity poiInfo) {
        if (!success || null == poiInfo || null == poiInfo.getPoint()) {
            Log.w(IVrBridgeConstant.TAG, "searchForEta empty result");
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createFailResponse("未找到" + mKeyword + "，试试别的吧"));
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
                Log.d(IVrBridgeConstant.TAG, "homeCompanyEta: " + homeCompanyEta);
                if (null != mRespCallback) {
                    mRespCallback.onResponse(CallResponse.createSuccessResponse(homeCompanyEta));
                }
            }).exceptionally(throwable -> {
                if (null != mRespCallback) {
                    mRespCallback.onResponse(CallResponse.createFailResponse("没有查询到相关信息，试试别的吧"));
                }
                return null;
            });
        }
    }

    /**
     * Poi排序.
     *
     * @param sessionId String，语音多轮对话保持一致性.
     * @param type 排序类别.
     * @param rule 排序方式.
     * @param respCallback RespCallback，执行结果回调.
     */
    public void sortPoi(final String sessionId, final String type, final String rule, final RespCallback respCallback) {
        Log.d(IVrBridgeConstant.TAG, "poiSort rule: " + rule);
        if (null == sessionId || !sessionId.equals(mSessionId)) {
            Log.d(IVrBridgeConstant.TAG, "not same sessionId");
            responseNotMatchId(respCallback);
            return;
        }
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            Log.d(IVrBridgeConstant.TAG, "poiSort searchResult is empty");
        }

        mSortValue = "";
        switch (type) {
            case IVrBridgeConstant.PoiSortType.DISTANCE:
                //距离，从近到远
                if(IVrBridgeConstant.PoiSortRule.ASCENDING.equals(rule)) {
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
                Log.d(IVrBridgeConstant.TAG, "unKnown sortType");
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
            respCallback.onResponse(CallResponse.createNotSupportResponse("不支持该排序，请继续选择"));
        }
    }

    /**
     * 处理排序筛选搜索结果.
     */
    private void dealSortResult() {
        if (null == mSearchResultList || mSearchResultList.isEmpty()) {
            Log.d(IVrBridgeConstant.TAG, "poiSort type: " + mSortValue + " search result empty");
            if (null != mRespCallback) {
                mRespCallback.onResponse(CallResponse.createFailResponse("排序搜索结果为空"));
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
                Log.i(IVrBridgeConstant.TAG, "unSupport sortValue: " + mSortValue);
                break;
        }

        if (appendHint) {
            final String distance = firstPoi.getDistance();
            builder.append(name).append("距您").append(distance).append("，去这里可以吗");
        }

        if (null != mRespCallback) {
            final String hintMsg = builder.toString();
            if (TextUtils.isEmpty(hintMsg)) {
                mRespCallback.onResponse(CallResponse.createFailResponse("不支持的排序类型"));
            } else {
                mRespCallback.onResponse(CallResponse.createSuccessResponse(hintMsg));
            }
        }
    }


}
