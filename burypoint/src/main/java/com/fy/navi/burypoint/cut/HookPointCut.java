package com.fy.navi.burypoint.cut;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.flyjingfish.android_aop_annotation.ProceedJoinPoint;
import com.flyjingfish.android_aop_annotation.base.BasePointCut;
import com.fy.navi.burypoint.BuryManager;
import com.fy.navi.burypoint.DataTrackUtils;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryParam;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.controller.BuryPointController;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;
import java.util.Objects;

public class HookPointCut implements BasePointCut<HookMethod> {

    @Nullable
    @Override
    public Object invoke(@NonNull ProceedJoinPoint proceedJoinPoint, @NonNull HookMethod hookMethod) throws Throwable {
        Object result = proceedJoinPoint.proceed();
        sendStructData(hookMethod);
        return result;
    }

    private void sendStructData(@NonNull HookMethod hookMethod) {

        if(!BuryManager.getInstance().getCar()) return;

        BuryProperty buryProperty = BuryPointController.getInstance().getBuryProps();
//        SensorsDataAPI gapi = OASAPI.sharedInstance();
        DataTrackUtils gapi = DataTrackUtils.getInstance();

        final String eventName = Objects.equals(hookMethod.eventName(), "") ? BuryPointController.getInstance().getEventName() : hookMethod.eventName();

        if(buryProperty != null && buryProperty.getParams() != null && !buryProperty.getParams().isEmpty()){
            List<BuryParam> params = buryProperty.getParams();
            JSONObject properties = new JSONObject();
            for (BuryParam param : params) {
                try {
                    properties.put(param.getKey(), param.getValue());
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }
//            gapi.track(eventName, hookMethod.sid(), hookMethod.svid(), properties.toString());
            gapi.track(eventName, properties.toString());

        } else {
            gapi.track(eventName, null);


//            gapi.track(eventName, hookMethod.sid(), hookMethod.svid());
//            gapi.flushSync();
        }
    }
}
