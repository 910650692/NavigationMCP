package com.fy.navi.burypoint.cut;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.bigtimes.sdk.OASAPI;
import com.flyjingfish.android_aop_annotation.ProceedJoinPoint;
import com.flyjingfish.android_aop_annotation.base.BasePointCut;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryParam;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.sensorsdata.analytics.android.sdk.SensorsDataAPI;
import com.sensorsdata.analytics.android.sdk.exceptions.BuriedPointModeNotMatchMethodException;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;
import java.util.Objects;

public class HookPointCut implements BasePointCut<HookMethod> {

    private static final String TAG = "HookPointCut";

    @Nullable
    @Override
    public Object invoke(@NonNull ProceedJoinPoint proceedJoinPoint, @NonNull HookMethod hookMethod) throws Throwable {
        Object result = proceedJoinPoint.proceed();
        sendStructData(hookMethod);
        return result;
    }

    private void sendStructData(@NonNull HookMethod hookMethod) {
        BuryProperty buryProperty = BuryPointController.getInstance().getBuryProps();
        SensorsDataAPI gapi = OASAPI.sharedInstance();

        final String eventName = Objects.equals(hookMethod.eventName(), "") ? BuryPointController.getInstance().getEventName() : hookMethod.eventName();

        Logger.d(TAG, "sendStructData: " + hookMethod.eventName() + " " + hookMethod.sid() + " " + hookMethod.svid());
        if(buryProperty != null && buryProperty.getParams() != null && !buryProperty.getParams().isEmpty()){
            List<BuryParam> params = buryProperty.getParams();
            JSONObject properties = new JSONObject();
            for (BuryParam param : params) {
                try {
                    Logger.d(TAG, "sendStructData: " + param.getKey() + " " + param.getValue());
                    properties.put(param.getKey(), param.getValue());
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }
            try {
                gapi.track(eventName, hookMethod.sid(), hookMethod.svid(), properties.toString());
            } catch (BuriedPointModeNotMatchMethodException e) {
                throw new RuntimeException(e);
            }

        } else {
            try {
                OASAPI.sharedInstance().track(eventName, hookMethod.sid(), hookMethod.svid());
                OASAPI.sharedInstance().flushSync();
            } catch (BuriedPointModeNotMatchMethodException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
