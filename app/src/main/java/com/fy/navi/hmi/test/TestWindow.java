package com.fy.navi.hmi.test;

import static androidx.core.app.ActivityCompat.startActivityForResult;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.Matrix;
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
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.LayoutTestBinding;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.engine.GaodeLogLevel;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.hud.HudPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.recorder.RecorderPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
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
        mBinding.naviPosRecord.setChecked(PositionPackage.getInstance().isOpenLocLog());
    }

    private void initData() {
        try {
            PackageManager packageManager = AppCache.getInstance().getMApplication().getPackageManager();
            PackageInfo packageInfo = packageManager.getPackageInfo(AppCache.getInstance().getMApplication().getPackageName(), 0);
            mBinding.testVersion.setText("versionName: " + packageInfo.versionName + "\n" +
                    "versionCode: " + packageInfo.getLongVersionCode() + "\n" +
                    "flavor: " + BuildConfig.FLAVOR + "\n" +
                    "buildType: " + BuildConfig.BUILD_TYPE);
        } catch (PackageManager.NameNotFoundException e) {
            Logger.e(TAG, "initData: ", e);
        }
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
        mLayoutParams.flags = WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL;
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
        mWindowManager.removeView(mBinding.getRoot());
        isShowing = false;
        HudPackage.getInstance().unInitHudService();
    }

    private void initAction() {
        mBinding.close.setOnClickListener(v -> removeViewFromWindow());
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

        mBinding.testShowDr.setOnClickListener(v -> {
            Activity mActivity = mActivityRef.get();
            removeViewFromWindow();
            DebugWindow.getInstance().show(mActivity);
        });

        mBinding.testHideDr.setOnClickListener(v -> {
            removeViewFromWindow();
            DebugWindow.getInstance().hide();
        });

        mBinding.naviPosRecord.setOnCheckedChangeListener((buttonView, checked) -> {
            if (buttonView.isPressed()) {
                if (checked) {
                    PositionPackage.getInstance().locationLogSwitch(true);
                } else {
                    PositionPackage.getInstance().locationLogSwitch(false);
                }
            }
        });

        mBinding.testNavLog.setOnCheckedChangeListener((buttonView, checked) -> {
            if (buttonView.isPressed()) {
                if (BuildConfig.DEBUG) {
                    Logger.switchLog(checked);
                } else {
                    //release 下log全关
                    Logger.switchLog(false);
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_NONE);
                }
            }
        });

        mBinding.testNaiLogLevel.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                Logger.setLogLevel(position + 2);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });


        mBinding.testGaodeLog.setOnCheckedChangeListener((buttonView, checked) -> {
            if (buttonView.isPressed()) {
                if (BuildConfig.DEBUG) {
                    if (checked) {
                        EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_VERBOSE);
                    } else {
                        EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_NONE);
                    }
                } else {
                    //release 下log全关
                    Logger.switchLog(false);
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_NONE);
                }
            }
        });

        mBinding.testGaodeLogLevel.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (position == 0) {
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_VERBOSE);
                } else if (position == 1) {
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_DEBUG);
                } else if (position == 2) {
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_INFO);
                } else if (position == 3) {
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_WARN);
                } else if (position == 4) {
                    EnginePackage.getInstance().switchLog(GaodeLogLevel.LOG_ERROR);
                }
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });

        mBinding.naviRecord.setChecked(RecorderPackage.getInstance().isRecording());
        mBinding.recordPlay.setChecked(RecorderPackage.getInstance().isPlaying());
        mBinding.naviRecord.setOnCheckedChangeListener((buttonView, checked) -> {
            if (buttonView.isPressed()) {
                Activity activity = mActivityRef.get();
                if (checked) {
                    if (RecorderPackage.getInstance().isPlaying()) {
                        RecorderPackage.getInstance().stopPlayback();
                        mBinding.recordPlay.setChecked(false);
                    }
                    mBinding.naviRecord.setText(activity.getString(R.string.test_stop_record));
                    RecorderPackage.getInstance().startRecorder();
                } else {
                    mBinding.naviRecord.setText(activity.getString(R.string.test_start_record));
                    RecorderPackage.getInstance().stopRecorder();
                }
            }
        });
        mBinding.recordPlay.setOnCheckedChangeListener((buttonView, checked) -> {
            if (buttonView.isPressed()) {
                Activity activity = mActivityRef.get();
                if (checked) {
                    if (RecorderPackage.getInstance().isRecording()) {
                        RecorderPackage.getInstance().stopRecorder();
                        mBinding.naviRecord.setChecked(false);
                    }
                    mBinding.recordPlay.setText(activity.getString(R.string.test_record_stop));
                    RecorderPackage.getInstance().startPlayback();
                } else {
                    mBinding.recordPlay.setText(activity.getString(R.string.test_record_play));
                    RecorderPackage.getInstance().stopPlayback();
                }
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

    private ArrayAdapter<String> createNaiAdapter() {
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

    private byte[] processPicture(byte[] bytes, int width, int height) {
        Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        bitmap.copyPixelsFromBuffer(ByteBuffer.wrap(bytes));
        // 翻转图像
        Matrix matrix = new Matrix();
        matrix.postScale(1, -1);
        matrix.postTranslate(bitmap.getWidth(), bitmap.getHeight());
        Bitmap flippedBitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);

        // 将翻转后的 Bitmap 转换为 byte[]
        ByteBuffer flippedBuffer = ByteBuffer.allocate(flippedBitmap.getByteCount());
        flippedBitmap.copyPixelsToBuffer(flippedBuffer);
        byte[] flippedBytes = flippedBuffer.array();

        // 提取有效数据
        int rowStride = ((width * 4 + 3) / 4) * 4;
        int validRowBytes = width * 4;
        byte[] croppedData = new byte[width * height * 4];
        ByteBuffer buffer = ByteBuffer.wrap(flippedBytes);
        for (int i = 0; i < height; i++) {
            int bufferPosition = i * rowStride;
            int arrayPosition = i * validRowBytes;
            buffer.position(bufferPosition);
            buffer.get(croppedData, arrayPosition, validRowBytes);
        }
        return croppedData;
    }
}