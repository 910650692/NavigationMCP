package com.fy.navi.hmi.test;

import static androidx.core.app.ActivityCompat.startActivityForResult;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.graphics.PixelFormat;
import android.net.Uri;
import android.provider.Settings;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import com.android.utils.log.Logger;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.hmi.databinding.LayoutTestBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.engine.GaodeLogLevel;
import com.fy.navi.service.define.setting.SettingConstant;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

public class TestWindow {
    private static final String TAG = TestWindow.class.getSimpleName();
    private WeakReference<Activity> mActivityRef;
    private WindowManager mWindowManager;
    private WindowManager.LayoutParams mLayoutParams;
    private LayoutTestBinding mBinding;
    private boolean isShowing;
    private static volatile TestWindow instance;

    private TestWindow() {
    }

    public static TestWindow getInstance() {
        if (instance == null) {
            synchronized (TestWindow.class) {
                if (instance == null) {
                    instance = new TestWindow();
                }
            }
        }
        return instance;
    }

    public void show(Activity activity) {
        if (isShowing || activity == null) {
            return;
        }
        mActivityRef = new WeakReference<>(activity);
        Activity mActivity = mActivityRef.get();
        if (mActivity == null) {
            return;
        }

        if (!checkOverlayPermission(activity)) {
            requestOverlayPermission(activity);
            return;
        }

        mBinding = LayoutTestBinding.inflate(LayoutInflater.from(activity));
        mWindowManager = (WindowManager) activity.getSystemService(Context.WINDOW_SERVICE);
        initLayoutParams();
        initAction();
        initData();
        addViewToWindow();
        initLogLevel();
    }

    private void initData() {
        ApplicationInfo applicationInfo = AppContext.getInstance().getMApplication().getApplicationInfo();
        String nativeLibraryDir = applicationInfo.nativeLibraryDir;
        mBinding.testSoLib.setText(nativeLibraryDir);
        mBinding.testNaiLogLevel.setAdapter(createNaiAdapter());
        mBinding.testGaodeLogLevel.setAdapter(createNaiAdapter());
    }

    private boolean checkOverlayPermission(Context context) {
        return Settings.canDrawOverlays(context);
    }

    private void requestOverlayPermission(Activity activity) {
        Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
                Uri.parse("package:" + activity.getPackageName()));
        startActivityForResult(activity, intent, 0, null);
    }

    private void initLayoutParams() {
        mLayoutParams = new WindowManager.LayoutParams();
        mLayoutParams.type = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        mLayoutParams.flags = WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL | WindowManager.LayoutParams.FLAG_LAYOUT_IN_SCREEN;
        mLayoutParams.gravity = Gravity.TOP | Gravity.END;
        mLayoutParams.width = WindowManager.LayoutParams.WRAP_CONTENT;
        mLayoutParams.height = WindowManager.LayoutParams.WRAP_CONTENT;
        mLayoutParams.format = PixelFormat.TRANSLUCENT;
    }

    private void addViewToWindow() {
        try {
            mWindowManager.addView(mBinding.getRoot(), mLayoutParams);
            isShowing = true;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void removeViewFromWindow() {
        if (!isShowing) {
            return;
        }
        Activity mActivity = mActivityRef.get();
        if (mActivity != null && mWindowManager != null) {
            mWindowManager.removeView(mBinding.getRoot());
        }
        isShowing = false;
    }

    private void initAction() {
        mBinding.close.setOnClickListener(v -> removeViewFromWindow());

        mBinding.testStartNavi.setOnClickListener(v -> NaviPackage.getInstance().startNavigation(SettingConstant.ISSIMULATEMODE));
        mBinding.testStopNavi.setOnClickListener(v -> NaviPackage.getInstance().stopNavigation());

        mBinding.testCurrentCityData.setOnClickListener(v -> MapDataPackage.getInstance().getCityInfo(310000));
        mBinding.testKeyCityData.setOnClickListener(v -> MapDataPackage.getInstance().searchAdCode("海"));

        mBinding.testCalibration.setOnClickListener(v -> {
            CalibrationPackage calibration = CalibrationPackage.getInstance();
            calibration.powerType();
            calibration.brand();
            calibration.model();
            calibration.enableApplicationNavigation();
            calibration.laneLevelNavigatioFuncEnable();
            calibration.v2xMapDisplayFuncEnable();
            calibration.speedLimitInformationSource();
            calibration.adasConfigurationInfomation();
            calibration.adasConfigurationType();
            calibration.rearSeatTouchPanelFuncEnable();
            calibration.hudFuncEnable();
            calibration.navigationDeflectionEnable();
            calibration.architecture();
            calibration.navigationPreConditionDataProvideEnable();
            calibration.navigaitonSupplier();
            calibration.highVoltageBatteryPropulsionTotalRangeNavi();
            calibration.poiSearchFuncEnable();
            calibration.scenarioEngineFuncEnable();
            calibration.globalSearchFuncEnable();
            calibration.teamTravelFuncEnable();
            calibration.bootAnimationReplacementFuncEnable();
            calibration.imeFuncEnable();
            calibration.wallpaperThemeFuncEnable();
            calibration.themeDefaultValue();
            calibration.slopeUpCostlist();
            calibration.slopeDownCostlist();
            calibration.transAccessCostlist();
            calibration.transDecessCostlist();
            calibration.speedCostlist();
            calibration.auxCostlist();
            calibration.vehicleWeight();
        });

        mBinding.testSignal.setOnClickListener(v -> {
            SignalPackage signal = SignalPackage.getInstance();
            signal.getBatteryEnergy();
            signal.getBatteryEnergyPercent();
            signal.getMaxBatteryEnergy();
            signal.getChargeSystemStatus();
            signal.getOutsideTemperature();
            signal.getSpeedOfVehicle();
            signal.getAcSwitchState();
            signal.getSystemState();
            signal.getRangeRemaining();
            signal.getHighVoltageBatteryPropulsionRange();
        });

        mBinding.testOpenClusterActivity.setOnClickListener(v -> {
            MyFsaService.getInstance().onReceiveRequest(9201, "23");
        });

        mBinding.testCloseClusterActivity.setOnClickListener(v -> {
            MyFsaService.getInstance().onReceiveRequest(9201, "24");
        });

        mBinding.testInsertData.setOnClickListener(v -> {
            SettingManager dbHelper = new SettingManager();
            dbHelper.insertValue("", "");
        });

        mBinding.testDeleteData.setOnClickListener(v -> {
            SettingManager dbHelper = new SettingManager();
            dbHelper.deleteAll();
        });

        mBinding.testShowDr.setOnClickListener(v -> {
            Activity mActivity = mActivityRef.get();
            removeViewFromWindow();
            DebugWindow.getInstance().show(mActivity);
        });

        mBinding.testHideDr.setOnClickListener(v -> {
            removeViewFromWindow();
            DebugWindow.getInstance().hide();
        });

        mBinding.testOpenLocLog.setOnClickListener(v -> {
            PositionPackage.getInstance().locationLogSwitch(true);
            removeViewFromWindow();
        });

        mBinding.testCloseLocLog.setOnClickListener(v -> {
            PositionPackage.getInstance().locationLogSwitch(false);
            removeViewFromWindow();
        });

        mBinding.testSelectText.setOnClickListener(v -> toggleSelection(mBinding.testSelectText));
        mBinding.testSelectTextBg.setOnClickListener(v -> toggleSelection(mBinding.testSelectTextBg));
        mBinding.testSelectTextBg1.setOnClickListener(v -> toggleSelection(mBinding.testSelectTextBg1));
        mBinding.testForegroundText.setOnClickListener(v -> toggleSelection(mBinding.testForegroundText));


        mBinding.testNavLog.setOnCheckedChangeListener((buttonView, checked) -> {
            if(buttonView.isPressed()){
                Logger.switchLog(checked);
            }
        });

        mBinding.testNaiLogLevel.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                Logger.setLogLevel(position+2);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });


        mBinding.testGaodeLog.setOnCheckedChangeListener((buttonView, checked) -> {
            if(buttonView.isPressed()){
                if(checked){
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_VERBOSE);
                }else {
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_NONE);
                }
            }
        });

        mBinding.testGaodeLogLevel.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if(position == 0){
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_VERBOSE);
                }else if(position == 1){
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_DEBUG);
                }else if(position == 2){
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_INFO);
                }else if(position == 3){
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_WARN);
                }else if(position == 4){
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_ERROR);
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });
    }

    private void initLogLevel() {
        Logger.setLogLevel(2);
        EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_VERBOSE);
    }

    private void toggleSelection(View view) {
        view.setSelected(!view.isSelected());
    }

    private ArrayAdapter<String> createNaiAdapter(){
        // 创建一个包含选项的列表
        List<String> options = new ArrayList<>();
        options.add("VERBOSE");
        options.add("DEBUG");
        options.add("INFO");
        options.add("WARN");
        options.add("ERROR");
        // 创建一个 ArrayAdapter 来为 Spinner 提供数据
        return new ArrayAdapter<>(mActivityRef.get(),
                android.R.layout.simple_spinner_item, options);
    }
}