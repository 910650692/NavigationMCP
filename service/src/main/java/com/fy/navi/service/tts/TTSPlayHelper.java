package com.fy.navi.service.tts;

import com.fy.navi.service.define.navi.PlayModule;

import java.util.Arrays;
import java.util.List;

public class TTSPlayHelper {

    private static final List<Integer> soundTypeEvenNopOpen = Arrays.asList(
            PlayModule.PlayModuleRoute,
            PlayModule.PlayModuleLocal,
            PlayModule.PlayModuleFreeWay,
            PlayModule.PlayModuleTime,
            PlayModule.PlayModuleCurve,
            PlayModule.PlayModuleTrafficStatus,
            PlayModule.PlayModuleWeatherPlay,
            PlayModule.PlayModuleLaneNavi
            );

    public static boolean allowToPlayWithNopOpen(int soundType){
        return soundTypeEvenNopOpen.contains(soundType);
    }
}
