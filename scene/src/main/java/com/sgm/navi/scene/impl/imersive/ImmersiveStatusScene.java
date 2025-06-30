package com.sgm.navi.scene.impl.imersive;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.define.map.MapType;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;


public class ImmersiveStatusScene {

    private Map<MapType, ImersiveStatus> imersiveStatusMap;
    private ConcurrentHashMap<String, IImmersiveStatusCallBack> immersiveStatusCallBacks;

    private ImmersiveStatusScene() {
        imersiveStatusMap = new HashMap<>();
        immersiveStatusCallBacks = new ConcurrentHashMap<>();
    }

    public void registerCallback(String key, IImmersiveStatusCallBack immersiveStatusCallBack) {
        immersiveStatusCallBacks.put(key, immersiveStatusCallBack);
    }

    public void unRegisterCallback(String key) {
        immersiveStatusCallBacks.remove(key);
    }

    public void setImmersiveStatus(MapType mapTypeId, ImersiveStatus imersiveStatus) {
        if (Logger.openLog) {
            StackTraceElement[] traceElements = Thread.currentThread().getStackTrace();
            StringBuilder stackTrace = new StringBuilder();
            for (StackTraceElement element : traceElements) {
                if (element != null) {
                    String elementStr = element.toString();
                    if (elementStr.contains("com.sgm")) {
                        stackTrace.append(elementStr).append("\n");
                    }
                }
            }
            Logger.d("ImmersiveStatusScene", stackTrace.toString());
        }
        synchronized (ImmersiveStatusScene.class) {
            imersiveStatusMap.put(mapTypeId, imersiveStatus);
            ThreadManager.getInstance().postDelay(()->{
                if (ConvertUtils.isEmpty(immersiveStatusCallBacks)) return;;
                for (IImmersiveStatusCallBack callback : immersiveStatusCallBacks.values()) {
                    if (!ConvertUtils.isEmpty(callback)) {
                        callback.onImmersiveStatusChange(mapTypeId, imersiveStatus);
                    }
                }
            },0);
        }
    }

    public ImersiveStatus getCurrentImersiveStatus(MapType mapTypeId) {
        if (imersiveStatusMap.containsKey(mapTypeId)){
            return imersiveStatusMap.get(mapTypeId);
        }
        return ImersiveStatus.IMERSIVE;
    }

    public static ImmersiveStatusScene getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final ImmersiveStatusScene ra = new ImmersiveStatusScene();
    }

    public interface IImmersiveStatusCallBack {
        void onImmersiveStatusChange(MapType mapTypeId, ImersiveStatus lastImersiveStatus);
    }
}
