package com.sgm.navi.service.adapter.aos.bls;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.BLAosService;
import com.autonavi.gbl.aosclient.model.GCoord3DDouble;
import com.autonavi.gbl.aosclient.model.GHolidayListRequestParam;
import com.autonavi.gbl.aosclient.model.GHolidayListResponseParam;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaDataRes;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaRequestParam;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaResponseParam;
import com.autonavi.gbl.aosclient.model.GRestrictCity;
import com.autonavi.gbl.aosclient.model.GRestrictRule;
import com.autonavi.gbl.aosclient.model.GRestrictRulePoints;
import com.autonavi.gbl.aosclient.model.GTrafficEventCommentRequestParam;
import com.autonavi.gbl.aosclient.model.GTrafficEventCommentResponseParam;
import com.autonavi.gbl.aosclient.model.GTrafficEventDetailRequestParam;
import com.autonavi.gbl.aosclient.model.GTrafficEventDetailResponseParam;
import com.autonavi.gbl.aosclient.model.GTrafficRestrictRequestParam;
import com.autonavi.gbl.aosclient.model.GTrafficRestrictResponseParam;
import com.autonavi.gbl.aosclient.model.GWsDynamicInfoEventPraiseStampStatusQueryRequestParam;
import com.autonavi.gbl.aosclient.model.GWsDynamicInfoEventPraiseStampStatusQueryResponseParam;
import com.autonavi.gbl.aosclient.observer.ICallBackHolidayList;
import com.autonavi.gbl.aosclient.observer.ICallBackReStrictedArea;
import com.autonavi.gbl.aosclient.observer.ICallBackTrafficEventComment;
import com.autonavi.gbl.aosclient.observer.ICallBackTrafficEventDetail;
import com.autonavi.gbl.aosclient.observer.ICallBackTrafficRestrict;
import com.autonavi.gbl.aosclient.observer.ICallBackWsDynamicInfoEventPraiseStampStatusQuery;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.BlToolPoiID;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.adapter.aos.BlAosHelper;
import com.sgm.navi.service.adapter.aos.IBlAosApi;
import com.sgm.navi.service.adapter.aos.QueryRestrictedObserver;
import com.sgm.navi.service.define.aos.FyCriticism;
import com.sgm.navi.service.define.aos.FyGTraEventDetail;
import com.sgm.navi.service.define.aos.FyTrafficUploadParameter;
import com.sgm.navi.service.define.aos.RestrictedArea;
import com.sgm.navi.service.define.aos.RestrictedEndNumberParam;
import com.sgm.navi.service.define.aos.RestrictedParam;
import com.sgm.navi.service.define.aos.RestrictedAreaDetail;
import com.sgm.navi.service.define.aos.TrafficRestrictResponseParam;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.route.RouteRestrictionParam;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/6
 */
public class BlAosAdapterImpl implements IBlAosApi, ICallBackReStrictedArea, ICallBackTrafficEventDetail, ICallBackTrafficEventComment, ICallBackWsDynamicInfoEventPraiseStampStatusQuery
, ICallBackHolidayList, ICallBackTrafficRestrict {
    private static final String TAG = BlAosAdapterImpl.class.getSimpleName();
    private BLAosService mBLAosService;
    private final Hashtable<String, QueryRestrictedObserver> restrictedObserver;
    private Hashtable<Long, RestrictedAreaDetail> restrictedAreaDetailMap;
    private Hashtable<Long, FyGTraEventDetail> trafficEventDetailMap;
    private Hashtable<Long, FyCriticism> fyCriticismMap;

    public BlAosAdapterImpl() {
        restrictedObserver = new Hashtable<>();
        restrictedAreaDetailMap = new Hashtable<>();
        trafficEventDetailMap = new Hashtable<>();
        fyCriticismMap = new Hashtable<>();
    }

    @Override
    public boolean initBlAosService() {
        mBLAosService = (BLAosService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.AosClientSingleServiceID);
        return isInit();
    }

    @Override
    public boolean isInit() {
        return !ConvertUtils.isEmpty(mBLAosService);
    }

    @Override
    public void addRestrictedObserver(String key, QueryRestrictedObserver observer) {
        restrictedObserver.put(key, observer);
    }

    @Override
    public void removeRestrictedObserver(String key) {
        ConvertUtils.remove(restrictedObserver, key);
    }

    @Override
    public long queryRestrictedInfo(RestrictedParam restrictedParam) {
        if (!isInit()) initBlAosService();
        if (!isInit()) initBlAosService();
        GReStrictedAreaRequestParam gReStrictedAreaRequestParam = GsonUtils.convertToT(restrictedParam, GReStrictedAreaRequestParam.class);
        long taskId = mBLAosService.sendReqReStrictedArea(gReStrictedAreaRequestParam, this);
        Logger.d(TAG, "请求任务Id", taskId);
        ConvertUtils.push(restrictedAreaDetailMap, taskId, new RestrictedAreaDetail());
        return taskId;
    }

    @Override
    public long queryRestrictedEndNumber(final RestrictedEndNumberParam restrictedParam) {
        if (!isInit()) initBlAosService();
        GTrafficRestrictRequestParam requestParam = new GTrafficRestrictRequestParam();
        requestParam.Adcode = restrictedParam.getAdcodes();
        requestParam.CarPlate = restrictedParam.getPlate();
        long taskId = mBLAosService.sendReqTrafficRestrict(requestParam, this);
        return taskId;
    }

    @Override
    public long queryTrafficEventInfo(String poiId, boolean isNeedConvert) {
        if (!isInit()) initBlAosService();
        GTrafficEventDetailRequestParam javaRequest = new GTrafficEventDetailRequestParam();
        javaRequest.eventid = isNeedConvert ? BlToolPoiID.poiIDToEventID(poiId) : poiId;
        long taskId = mBLAosService.sendReqTrafficEventDetail(javaRequest, this);
        Logger.d(TAG, "queryTrafficEventInfo--请求任务Id", taskId);
        ConvertUtils.push(trafficEventDetailMap, taskId, new FyGTraEventDetail());
        return taskId;
    }

    @Override
    public void onRecvAck(GReStrictedAreaResponseParam gReStrictedAreaResponseParam) {
        Logger.d(TAG, "请求结果", gReStrictedAreaResponseParam.code, gReStrictedAreaResponseParam.mHttpAckCode, gReStrictedAreaResponseParam.message);
        if (200 != gReStrictedAreaResponseParam.mHttpAckCode) return;
        long taskId = gReStrictedAreaResponseParam.mReqHandle;
        Logger.d(TAG, "start callBack to hmi:" , (restrictedObserver.size()));
        RouteRestrictionParam param = new RouteRestrictionParam();
        param.setMRestrictedArea(getRestrictedAreaDetail(gReStrictedAreaResponseParam, taskId));
        param.setMReStrictedAreaResponseParam(gReStrictedAreaResponseParam);
        for (QueryRestrictedObserver resultObserver : restrictedObserver.values()) {
            if (resultObserver == null) continue;
            resultObserver.onDrawRestrictionAndDetails(param);
        }
    }

    @Override
    public void onRecvAck(GTrafficEventDetailResponseParam gTrafficEventDetailResponseParam) {
        Logger.i(TAG, "onRecvAck----GTrafficEventDetailResponseParam");
        long taskId = gTrafficEventDetailResponseParam.mReqHandle;
        trafficEventDetailMap.get(taskId);
        FyGTraEventDetail result = new FyGTraEventDetail();
        result.taskId = taskId;
        if (200 == gTrafficEventDetailResponseParam.mHttpAckCode) {
            result = BlAosHelper.getInstance().ampConvertMine(gTrafficEventDetailResponseParam.EventData, taskId);
        } else {
            result.isRequestSuccess = false;
        }
        for (QueryRestrictedObserver resultObserver : restrictedObserver.values()) {
            if (resultObserver == null) continue;
            resultObserver.onTrafficQueryDetail(result);
        }
    }

    @Override
    public long updateTrafficEvent(FyTrafficUploadParameter parameter) {
        GTrafficEventCommentRequestParam javaRequest = new GTrafficEventCommentRequestParam();
        javaRequest.Type = parameter.type;
        javaRequest.Id = parameter.eventId;
        javaRequest.Lon = parameter.lon;
        javaRequest.Lat = parameter.lat;
        return mBLAosService.sendReqTrafficEventComment(javaRequest, this);
    }

    @Nullable
    private RestrictedArea getRestrictedAreaDetail(GReStrictedAreaResponseParam param, long taskId) {
        RestrictedArea restrictedArea = new RestrictedArea();
        ArrayList<String> cityNames = new ArrayList<>();
        ArrayList<Integer> cityPositions = new ArrayList<>();
        ArrayList<ArrayList<RestrictedAreaDetail>> restrictedAreaDetailsList = new ArrayList<>();
        GReStrictedAreaDataRes gReStrictedAreaDataRes = param.data;
        restrictedArea.setMRequestId(taskId);
        ArrayList<GRestrictCity> typelist = new ArrayList<>();
        ArrayList<ArrayList<PreviewParams.PointD>> pointLists = new ArrayList<>();
        if (gReStrictedAreaDataRes.mType == 7) {
            typelist.addAll(gReStrictedAreaDataRes.mCityAllRule.typelist);
        } else if (gReStrictedAreaDataRes.mType == 9) {
            typelist.addAll(gReStrictedAreaDataRes.mDataRule.cities);
        }
        if (!ConvertUtils.isEmpty(typelist)) {
            for (GRestrictCity gRestrictCity: typelist) {
                if (!cityNames.contains(gRestrictCity.cityName)) {
                    cityNames.add(gRestrictCity.cityName);
                    cityPositions.add(typelist.indexOf(gRestrictCity));
                    ArrayList<GRestrictRule> restrictRules = gRestrictCity.rules;
                    ArrayList<RestrictedAreaDetail> restrictedAreaDetails = new ArrayList<>();
                    ArrayList<PreviewParams.PointD> pointDS = new ArrayList<>();
                    if (!ConvertUtils.isEmpty(restrictRules)) {
                        for (GRestrictRule restrictRule : restrictRules) {
                            int index = restrictRules.indexOf(restrictRule) + 1;
                            RestrictedAreaDetail restrictedAreaDetail = new RestrictedAreaDetail();
                            restrictedAreaDetail.setMTitle("政策" + index);
                            restrictedAreaDetail.setMTime(restrictRule.time);
                            restrictedAreaDetail.setMDesc(restrictRule.desc);
                            restrictedAreaDetail.setMSummary(restrictRule.summary);
                            restrictedAreaDetail.setMEffect(restrictRule.effect);
                            restrictedAreaDetails.add(restrictedAreaDetail);
                            pointDS.addAll(getPointList(restrictRule));
                        }
                    }
                    pointLists.add(pointDS);
                    restrictedAreaDetailsList.add(restrictedAreaDetails);
                }
            }
            Logger.d(TAG, "getRestrictedAreaDetail:" , cityNames);
            restrictedArea.setMPointList(pointLists);
            restrictedArea.setMCityNames(cityNames);
            restrictedArea.setMCityPosition(cityPositions);
            restrictedArea.setMRestrictedAreaDetails(restrictedAreaDetailsList);
        }
        return restrictedArea;
    }

    private List<PreviewParams.PointD> getPointList(GRestrictRule restrictRule) {
        List<PreviewParams.PointD> list = new ArrayList<>();
        ArrayList<GRestrictRulePoints> linepoints = restrictRule.linepoints;
        if (!ConvertUtils.isEmpty(linepoints)) {
            for (GRestrictRulePoints restrictRulePoints : linepoints) {
                if (!ConvertUtils.isEmpty(restrictRulePoints.lstPoints)) {
                    ArrayList<GCoord3DDouble> lstPoints = restrictRulePoints.lstPoints;
                    for (GCoord3DDouble coord3DDouble : lstPoints) {
                        PreviewParams.PointD pointD = new PreviewParams.PointD();
                        pointD.x = coord3DDouble.lon;
                        pointD.y = coord3DDouble.lat;
                        list.add(pointD);
                    }
                }
            }
        }
        ArrayList<GRestrictRulePoints> areapoints = restrictRule.areapoints;
        if (!ConvertUtils.isEmpty(areapoints)) {
            for (GRestrictRulePoints restrictRulePoints : areapoints) {
                if (!ConvertUtils.isEmpty(restrictRulePoints.lstPoints)) {
                    ArrayList<GCoord3DDouble> lstPoints = restrictRulePoints.lstPoints;
                    for (GCoord3DDouble coord3DDouble : lstPoints) {
                        PreviewParams.PointD pointD = new PreviewParams.PointD();
                        pointD.x = coord3DDouble.lon;
                        pointD.y = coord3DDouble.lat;
                        list.add(pointD);
                    }
                }
            }
        }
        return list;
    }

    @Override
    public void onRecvAck(GTrafficEventCommentResponseParam gTrafficEventCommentResponseParam) {
        Logger.i(TAG, "onRecvAck-赞/踩-回调:" , gTrafficEventCommentResponseParam.message);
        boolean isSuccess = gTrafficEventCommentResponseParam.mHttpAckCode == 200;
        for (QueryRestrictedObserver resultObserver : restrictedObserver.values()) {
            if (resultObserver == null) continue;
            resultObserver.onTrafficUploadFinished(isSuccess);
        }
    }

    @Override
    public long queryDynamicInfoEvent(String trafficEventId) {
        GWsDynamicInfoEventPraiseStampStatusQueryRequestParam param = new GWsDynamicInfoEventPraiseStampStatusQueryRequestParam();
        param.eventId = trafficEventId;
        long taskId = mBLAosService.sendReqWsDynamicInfoEventPraiseStampStatusQuery(param, this);
        ConvertUtils.push(fyCriticismMap, taskId, new FyCriticism());
        return taskId;
    }

    @Override
    public void onRecvAck(GWsDynamicInfoEventPraiseStampStatusQueryResponseParam responseParam) {
        Logger.i(TAG, "onRecvAck-赞/踩查询结果：" , responseParam.status, "msg:" , responseParam.message);
        FyCriticism fyCriticism = new FyCriticism();
        fyCriticism.taskId = responseParam.mReqHandle;
        fyCriticism.status = responseParam.status;
        fyCriticism.isRequestSuccess = responseParam.mHttpAckCode == 200;
        for (QueryRestrictedObserver resultObserver : restrictedObserver.values()) {
            if (resultObserver == null) continue;
            resultObserver.onDynamicPraiseQueryFinished(fyCriticism);
        }
    }

    @Override
    public void sendReqHolidayList(){
        GHolidayListRequestParam param = new GHolidayListRequestParam();
        mBLAosService.sendReqHolidayList(param,this);
    }

    @Override
    public void onRecvAck(GHolidayListResponseParam gHolidayListResponseParam) {
        for (QueryRestrictedObserver resultObserver : restrictedObserver.values()) {
            if (resultObserver == null) continue;
            resultObserver.onRecvAck(gHolidayListResponseParam.lstHoliday);
        }
    }

    @Override
    public void onRecvAck(final GTrafficRestrictResponseParam gTrafficRestrictResponseParam) {
        Logger.d(TAG, "请求结果", gTrafficRestrictResponseParam.code, gTrafficRestrictResponseParam.mHttpAckCode, gTrafficRestrictResponseParam.message);
        if (200 != gTrafficRestrictResponseParam.mHttpAckCode) return;
        long taskId = gTrafficRestrictResponseParam.mReqHandle;
        TrafficRestrictResponseParam trafficRestrictResponseParam = new TrafficRestrictResponseParam();
        trafficRestrictResponseParam.setTaskId(taskId);
        trafficRestrictResponseParam.setPlateNo(gTrafficRestrictResponseParam.Restrict.m_plateNo);
        trafficRestrictResponseParam.setInfo(gTrafficRestrictResponseParam.Restrict.m_info);
        trafficRestrictResponseParam.setCityFlag(gTrafficRestrictResponseParam.Restrict.m_cityFlag);
        trafficRestrictResponseParam.setRestrictFlag(gTrafficRestrictResponseParam.Restrict.m_restrictFlag);
        for (QueryRestrictedObserver resultObserver : restrictedObserver.values()) {
            if (resultObserver == null) continue;
            resultObserver.onTrafficRestrict(trafficRestrictResponseParam);
        }
    }
}