package com.fy.navi.hmi.launcher;

import android.app.Service;
import android.content.Intent;
import android.graphics.PixelFormat;
import android.os.Build;
import android.os.IBinder;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.Nullable;

import com.fy.navi.hmi.R;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/24
 * Description: [在这里描述文件功能]
 */
public class LauncherWindowService extends Service {
    private WindowManager mWindowManager;
    private View mView;
    private WindowManager.LayoutParams mLayoutParams;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mWindowManager = (WindowManager) getSystemService(WINDOW_SERVICE);
        mView = LayoutInflater.from(this).inflate(R.layout.floating_window_layout, null);

        mLayoutParams = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.WRAP_CONTENT,
                Build.VERSION.SDK_INT >= Build.VERSION_CODES.O ?
                        WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY :
                        WindowManager.LayoutParams.TYPE_PHONE,
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,
                PixelFormat.TRANSLUCENT
        );

        mLayoutParams.gravity = Gravity.TOP | Gravity.START;
        mLayoutParams.x = 0;
        mLayoutParams.y = 0;
        mWindowManager.addView(mView, mLayoutParams);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mWindowManager != null && mView != null) {
            mWindowManager.removeView(mView);
        }
    }
}
