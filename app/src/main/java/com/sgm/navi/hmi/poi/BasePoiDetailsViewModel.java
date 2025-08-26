package com.sgm.navi.hmi.poi;

import static com.android.utils.TimeUtils.formatTimeRange;
import static com.android.utils.TimeUtils.isCurrentTimeInRange;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.CostTime;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.ReservationInfo;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
public class BasePoiDetailsViewModel extends BaseViewModel<PoiDetailsFragment, PoiDetailsModel> {
    private SearchResultEntity mSearchResultEntity;
    private int mTaskId;
    public BasePoiDetailsViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected PoiDetailsModel initModel() {
        return new PoiDetailsModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    /**
     * 搜索结果回调
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void onSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mView.onSearchResult(taskId, searchResultEntity);
    }

    /**
     * 搜索图层子点点击事件
     * @param index 点击下标
     */
    public void onMarkChildClickCallBack(final int index) {
        mView.onMarkChildClickCallBack(index);
    }

    public Action getRootClick() {
        return mRootClick;
    }

    private final Action mRootClick = () -> {
    };

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }

    /**
     * 判断是否在自车位
     * @return 是否在自车位
     */
    public boolean calcDistanceBetweenPoints(){
        return mModel.calcStraightDistance();
    }


    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        mModel.onReStoreFragment();
    }

    public void onSilentSearchResult(int taskId,SearchResultEntity searchResultEntity){
        mView.onSilentSearchResult(taskId,searchResultEntity);
    }

    /**
     * poi详情查询接口
     * 已登录: 详情 -》 收藏列表 -》预约状态
     * 未登录: 详情
     * @param taskId 任务id
     * @param result 详情数据
     */
    public void notifyNetSearchResult(int taskId,BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "code" , result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            JSONObject jsonObject = null;
            try {
                jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                // 处理经纬度
                GeoPoint point = new GeoPoint();
                point.setLat(ConvertUtils.str2Double(jsonObject.getString("stationLat")));
                point.setLon(ConvertUtils.str2Double(jsonObject.getString("stationLng")));
                ArrayList<ChargeInfo> chargeList = new ArrayList<>();
                ArrayList<PoiInfoEntity> poiInfoEntityList = new ArrayList<>();
                ChargeInfo chargeInfo = GsonUtils.fromJson(GsonUtils.toJson(result.getDataSet()),ChargeInfo.class);
                chargeInfo.setSlow_total(chargeInfo.getSlowChargingTotal())
                        .setSlow_free(chargeInfo.getSlowChargingFree())
                        .setFast_total(chargeInfo.getFastChargingTotal())
                        .setFast_free(chargeInfo.getFastChargingFree())
                        .setCurrentServicePrice(chargeInfo.getParkFee());
                CostTime currentTime = getCurrentElePrice(chargeInfo.getCostItem());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"currentTime: ",currentTime.getTime());
                chargeInfo.setCurrentElePrice(currentTime.getPrice());
                chargeList.add(chargeInfo);
                PoiInfoEntity poiInfoEntity = GsonUtils.fromJson(GsonUtils.toJson(result.getDataSet()),PoiInfoEntity.class);
                poiInfoEntity.setChargeInfoList(chargeList)
                        .setPoint(point)
                        .setName(poiInfoEntity.getStationName())
                        .setAddress(poiInfoEntity.getStationAddress())
                        .setPhone(poiInfoEntity.getStationTel())
                        .setBusinessTime(poiInfoEntity.getStationBusinessTime())
                        .setPointTypeCode("011100");
                if(!ConvertUtils.isEmpty(poiInfoEntity.getPictures())){
                    poiInfoEntity.setImageUrl(poiInfoEntity.getPictures().get(0));
                }
                poiInfoEntityList.add(poiInfoEntity);
                SearchResultEntity searchResultEntity = new SearchResultEntity()
                        .setIsNetData(true)
                        .setPoiList(poiInfoEntityList)
                        .setPoiType(1);
                mSearchResultEntity = searchResultEntity;
                mTaskId = taskId;
                if(mModel.isSGMLogin()){
                    // 查询收藏列表，确认该poi的收藏状态
                    mView.onNetSearchResult();
                }else{
                    mView.onSearchResult(taskId,searchResultEntity);
                    mModel.createPoiLabel(searchResultEntity);
                }
            } catch (JSONException e) {
                notifyNetSearchResultError(taskId,e.getMessage());
            }
        }else{
            notifyNetSearchResultError(taskId,result.getMessage());
        }
    }

    public void notifyCollectStatus(int taskId,BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "code" + result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            mView.onNotifyCollectStatus(taskId);
        }else{
            notifyCollectStatusError(taskId,result.getMessage());
        }

    }

    /**
     * 查询收藏列表，根据poiId判断该poi点是否已收藏
     * @param result 收藏列表
     */
    public void notifyCollectList(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"code",result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            ArrayList<PoiInfoEntity> list = new ArrayList<>();
            // 回调出的数据转换List
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("items");
                if(jsonArray.length() > 0 && !ConvertUtils.isNull(mSearchResultEntity)){
                    for (int j = 0; j < mSearchResultEntity.getPoiList().size(); j++) {
                        for (int i = 0; i < jsonArray.length(); i++) {
                            JSONObject object = new JSONObject(String.valueOf(jsonArray.get(i)));
                            String searchPoiId = mSearchResultEntity.getPoiList().get(j).getOperatorId();
                            String collectPoiId = object.getString("operatorId");
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"operatorId: ",searchPoiId);
                            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"operatorId string: ",collectPoiId);
                            mSearchResultEntity.getPoiList().get(j).setIsCollect(collectPoiId.equals(searchPoiId));
                        }
                    }
                }
                // 继续查询预约状态
                mView.searchReservation();
            } catch (JSONException e) {
                notifyCollectListError(e.getMessage());
            }
        }else{
            notifyCollectListError(result.getMessage());
        }
    }

    /**
     * 查询poi预约状态，详情查询链路最后一环，查询完成后，通知页面详情数据
     * @param result 预约单数据
     */
    public void notifyReservationList(BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"code",result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            // 回调出的数据转换List
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("resultList");
                String userId = AccountPackage.getInstance().getUserId();
                for (int i = 0; i < jsonArray.length(); i++) {
                    ReservationInfo reservationInfo = GsonUtils.fromJson(String.valueOf(jsonArray.get(i)),ReservationInfo.class);
                    if(reservationInfo.getmUserId().equals(userId) && reservationInfo.getmStatus() == 1){
                        mSearchResultEntity.getPoiList().get(0).setReservationInfo(reservationInfo);
                    }
                }
                final ThreadManager threadManager = ThreadManager.getInstance();
                threadManager.postUi(() -> {
                    mView.onSearchResult(mTaskId,mSearchResultEntity);
                    mModel.createPoiLabel(mSearchResultEntity);
                });
            } catch (JSONException e) {
                notifyReservationListError(result.getMessage());
            }
        }else{
            notifyReservationListError(result.getMessage());
        }
    }

    /**
     * 查询详情错误
     * @param taskId 任务id
     * @param message 错误信息
     */
    public void notifyNetSearchResultError(int taskId, String message){
        mView.onNetSearchResultError(taskId,message);
    }

    /**
     * 自查询：查询收藏列表失败，和详情查询共用同一个taskId
     * @param message 错误消息
     */
    public void notifyCollectListError(String message){
        notifyNetSearchResultError(mTaskId,message);
    }

    /**
     * 自查询：查询预约单列表失败，和详情查询共用同一个taskId
     * @param message 错误消息
     */
    public void notifyReservationListError(String message){
        notifyNetSearchResultError(mTaskId,message);
    }

    public void notifyCollectStatusError(int taskId, String message){
        mView.onNotifyCollectStatusError(taskId,message);
    }

    private CostTime getCurrentElePrice(ArrayList<CostTime> costTimes) {
        CostTime currentCostTime = new CostTime();
        if (!ConvertUtils.isEmpty(costTimes)) {
            for (int i = 0; i < costTimes.size(); i++) {
                String time = costTimes.get(i).getTime();
                if(!ConvertUtils.isEmpty(time)){
                    time = formatTimeRange(time);
                }
                if (isCurrentTimeInRange(time)) {
                    currentCostTime = costTimes.get(i);
                }
            }
        }
        return currentCostTime;
    }

    public void searchCollectList(AccessTokenParam param){
        mModel.searchCollectList(param);
    }

    public void searchReservation(AccessTokenParam param){
        mModel.queryReservation(param);
    }

    public void setIsSearchPoiDetailsFragment(boolean isSearchPoiDetailsFragment) {
        MapPackage.getInstance().setIsSearchPoiDetailsFragment(isSearchPoiDetailsFragment);
    }

    public void setPoiType(int poiType) {
        if (mModel != null) {
            mModel.setPoiType(poiType);
        }
    }

    public int getPoiType() {
        if (mModel != null) {
            return mModel.getPoiType();
        }
        return 1;
    }

    public void setPoiInfoEntity(PoiInfoEntity poiInfoEntity) {
        if (mModel != null) {
            mModel.setPoiInfoEntity(poiInfoEntity);
        }
    }

    public PoiInfoEntity getPoiInfoEntity() {
        if (mModel != null) {
            return mModel.getPoiInfoEntity();
        }
        return null;
    }

    public void setIsLoading(int loadingStatus) {
        if (mModel != null) {
            mModel.setIsLoading(loadingStatus);
        }
    }

    public int getIsLoading() {
        if (mModel != null) {
            return mModel.isLoading();
        }
        return 0;
    }

    public SearchResultEntity getSearchResultEntity() {
        if (mModel != null) {
            return mModel.getSearchResultEntity();
        }
        return null;
    }

    public void setSearchResultEntity(SearchResultEntity searchResultEntity) {
        if (mModel != null) {
            mModel.setSearchResultEntity(searchResultEntity);
        }
    }
}
