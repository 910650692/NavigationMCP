package com.sgm.navi.hmi.test;

import static androidx.core.app.ActivityCompat.startActivityForResult;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.PixelFormat;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;

import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.databinding.LayoutDebugBinding;
import com.sgm.navi.service.adapter.position.PositionConstant;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;

import java.text.SimpleDateFormat;
import java.util.Date;

public class DebugWindow implements IPositionPackageCallback {

    private static final String TAG = DebugWindow.class.getSimpleName();
    private WindowManager mWindowManager;
    private WindowManager.LayoutParams mLayoutParams;
    private LayoutDebugBinding mBinding;
    private boolean isShow;

    private final Handler timeHandler;
    private final Runnable timeUpdateRunnable = new Runnable() {
        @Override
        public void run() {
            updateTime();
            timeHandler.postDelayed(this, 1000); // 每秒更新一次
        }
    };


    private DebugWindow() {
        timeHandler = new Handler(Looper.getMainLooper());
        PositionPackage.getInstance().registerCallBack(this);
    }

    public void show(Activity activity) {
        if (isShow) {
            return;
        }
        if (!Settings.canDrawOverlays(activity)) {
            Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
                    Uri.parse("package:" + ((Context) activity).getPackageName()));
            startActivityForResult(activity, intent, 0, null);
            return;
        }
        mBinding = LayoutDebugBinding.inflate(LayoutInflater.from(activity));
        mWindowManager = (WindowManager) activity.getSystemService(Context.WINDOW_SERVICE);
        initLayoutParams();
        mWindowManager.addView(mBinding.getRoot(), mLayoutParams);
        startTimeUpdate();
        isShow = true;
        PositionPackage.getInstance().setDrEnable(true);
    }

    public void hide() {
        if (mWindowManager != null && mBinding != null) {
            stopTimeUpdate();
            isShow = false;
            mWindowManager.removeView(mBinding.getRoot());
        }
        PositionPackage.getInstance().setDrEnable(false);
    }

    private void initLayoutParams() {
        mLayoutParams = new WindowManager.LayoutParams();
        mLayoutParams.flags = WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL;
        mLayoutParams.gravity = Gravity.TOP | Gravity.CENTER;
        mLayoutParams.width = WindowManager.LayoutParams.WRAP_CONTENT;
        mLayoutParams.height = WindowManager.LayoutParams.WRAP_CONTENT;
        mLayoutParams.format = PixelFormat.TRANSLUCENT;
    }

    private void startTimeUpdate() {
        updateTime();
        timeHandler.postDelayed(timeUpdateRunnable, 1000);
    }

    private void stopTimeUpdate() {
        if (timeHandler != null) {
            timeHandler.removeCallbacks(timeUpdateRunnable);
        }
    }

    private void updateTime() {
        @SuppressLint("SimpleDateFormat")
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss");
        String currentTime = sdf.format(new Date());
        mBinding.time.setText(currentTime);
    }

    @Override
    public void onLocAnalysisResult(@PositionConstant.DRDebugEvent int infoType, String info) {
        ThreadManager.getInstance().postUi(() -> {
            updateDebugDrinfo(infoType, info);
        });
    }

    public void updateDebugDrinfo(int type, String msg) {
        if (mBinding == null) {
            return;
        }
        if (type == PositionConstant.DRDebugEvent.DR_TYPE_SENSOR) {
            mBinding.sensorTv.setText(msg);
        } else if (type == PositionConstant.DRDebugEvent.DR_LOSS_RATE) {
            mBinding.lossRateTv.setText(msg);
        }

        if (mBinding.drLl.getVisibility() != View.VISIBLE) {
            mBinding.drLl.setVisibility(View.VISIBLE);
        }
    }

    public static final class SInstanceHolder {
        public static final DebugWindow INSTANCE = new DebugWindow();
    }

    public static DebugWindow getInstance() {
        return SInstanceHolder.INSTANCE;
    }

}
