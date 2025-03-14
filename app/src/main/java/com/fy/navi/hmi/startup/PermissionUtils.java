package com.fy.navi.hmi.startup;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.media.projection.MediaProjectionManager;
import android.net.Uri;
import android.os.Environment;
import android.provider.Settings;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class PermissionUtils {
    private static final String TAG = PermissionUtils.class.getSimpleName();
    public static final int REQUEST_PERMISSION_CODE = 0;
    public static final int REQUEST_PERMISSION_EXTERNAL_CODE = 1;
    public static final int REQUEST_PERMISSION_OVERLAY_CODE = 2;
    public static final int REQUEST_PERMISSION_MEDIA_PROJECTION = 3;
    private static final int DENIED_PERMISSION_REQUEST_NUM = 1;
    private static final String SPEED_PERMISSION = "android.car.permission.CAR_SPEED";
    private int deniedPermissionRequestNum = 0;
    private Activity context;
    private MediaProjectionManager mProjectionManager;
    private PermissionsObserver permissionsObserver;
    private final List<String> permissionList = new ArrayList<>();
    private final List<String> deniedPermission = new ArrayList<>();
    private final String[] permissionArray = new String[]{Manifest.permission.READ_PHONE_STATE,
            Manifest.permission.ACCESS_FINE_LOCATION,
            Manifest.permission.MANAGE_EXTERNAL_STORAGE,
            Manifest.permission.CALL_PHONE,
            Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
            Context.MEDIA_PROJECTION_SERVICE};
    private boolean isMediaProjectionPermission = true; // TODO 启动HUD功能时改为false

    public void setPermissionsObserver(PermissionsObserver permissionsObserver) {
        this.permissionsObserver = permissionsObserver;
    }

    public boolean checkoutPermission() {
        permissionList.clear();
        deniedPermission.clear();
        for (String permission : permissionArray) {
            if (!checkoutPermission(permission)) permissionList.add(permission);
        }
        if(DeviceUtils.isCar(AppContext.mApplication)){
            Logger.i(TAG, "current device type is car");
            if (!checkoutPermission(SPEED_PERMISSION)) permissionList.add(SPEED_PERMISSION);
        }else {
            Logger.i(TAG, "current device type is pad");
        }
        return ConvertUtils.isEmpty(permissionList);
    }

    public boolean checkoutPermission(String permission) {
        if (ConvertUtils.isEmpty(permission)) return true;
        if (ConvertUtils.equals(permission, Manifest.permission.MANAGE_EXTERNAL_STORAGE)) {
            return Environment.isExternalStorageManager();
        }
        if (ConvertUtils.equals(permission, Settings.ACTION_MANAGE_OVERLAY_PERMISSION)) {
            return Settings.canDrawOverlays(AppContext.mContext);
        }
        if (ConvertUtils.equals(permission, Context.MEDIA_PROJECTION_SERVICE)) {
            return isMediaProjectionPermission;
        }
        return AppContext.mApplication.checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED;
    }

    public void requestPermission() {
        if (ConvertUtils.isEmpty(permissionList)) requestDeniedPermission();
        else requestPermission(permissionList.get(0));
    }

    public void requestDeniedPermission() {
        if (ConvertUtils.isEmpty(deniedPermission)) {
            permissionsObserver.onPermissionsSuccess();
            return;
        }
        if (deniedPermissionRequestNum >= DENIED_PERMISSION_REQUEST_NUM) {
            permissionsObserver.onPermissionsFail();
            return;
        }
        deniedPermissionRequestNum += 1;
        permissionList.clear();
        permissionList.addAll(deniedPermission);
        deniedPermission.clear();
        requestPermission(permissionList.get(0));
    }

    public void requestPermission(String permission) {
        permissionList.remove(permission);
        if (ConvertUtils.isEmpty(permission)) return;
        Logger.i(TAG, "current permission-> " + permission);
        if (ConvertUtils.equals(permission, Manifest.permission.MANAGE_EXTERNAL_STORAGE) && !Environment.isExternalStorageManager()) {
            Logger.i(TAG, "申请所有文件读写权限");
            requestManageExternal();
        } else if (ConvertUtils.equals(permission, Settings.ACTION_MANAGE_OVERLAY_PERMISSION)) {
            Logger.i(TAG, "申请悬浮窗权限");
            requestManageOverlay();
        } else if (ConvertUtils.equals(permission, Context.MEDIA_PROJECTION_SERVICE)) {
            Logger.i(TAG, "申请媒体投影权限");
            requestMediaProjection();
        } else {
            ActivityCompat.requestPermissions(StackManager.getInstance().getMainCurrentActivity(),
                    new String[]{permission}, REQUEST_PERMISSION_CODE);
        }
    }

    public void requestManageExternal(){
        PermissionDialog permissionDialog = new PermissionDialog.Build(context)
                .setTitle("申请文件读写权限")
                .setContent("地图导航中所需权限，如果没有该权限将可能会造成地图的无法使用，点击确定跳转到权限申请界面，点击取消可能会造成地图无法使用")
                .setDialogObserver(new IBaseDialogClickListener() {

                    @Override
                    public void onCancelClick() {
                        deniedPermission.add(Manifest.permission.MANAGE_EXTERNAL_STORAGE);
                        requestPermission();
                    }

                    @Override
                    public void onCommitClick() {
                        Intent intent = new Intent(Settings.ACTION_MANAGE_ALL_FILES_ACCESS_PERMISSION);
                        ActivityCompat.startActivityForResult(context, intent, REQUEST_PERMISSION_EXTERNAL_CODE, null);
                    }
                })
                .build();
        permissionDialog.show();
    }

    public void requestManageOverlay(){
        PermissionDialog permissionDialog = new PermissionDialog.Build(context)
                .setTitle("申请悬浮窗权限")
                .setContent("地图导航中所需权限，如果没有该权限将可能会造成地图的无法使用，点击确定跳转到权限申请界面，点击取消可能会造成地图无法使用")
                .setDialogObserver(new IBaseDialogClickListener() {

                    @Override
                    public void onCancelClick() {
                        deniedPermission.add(Settings.ACTION_MANAGE_OVERLAY_PERMISSION);
                        requestPermission();
                    }

                    @Override
                    public void onCommitClick() {
                        Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
                                Uri.parse("package:" + AppContext.mContext.getPackageName()));
                        ActivityCompat.startActivityForResult(context, intent, REQUEST_PERMISSION_OVERLAY_CODE, null);
                    }
                })
                .build();
        permissionDialog.show();
    }

    public void requestMediaProjection(){
        PermissionDialog permissionDialog = new PermissionDialog.Build(context)
                .setTitle("申请媒体投影权限")
                .setContent("地图导航中所需权限，如果没有该权限将可能会造成地图的无法使用，点击确定跳转到权限申请界面，点击取消可能会造成地图无法使用")
                .setDialogObserver(new IBaseDialogClickListener() {

                    @Override
                    public void onCancelClick() {
                        deniedPermission.add(Context.MEDIA_PROJECTION_SERVICE);
                        requestPermission();
                    }

                    @Override
                    public void onCommitClick() {
                        mProjectionManager = (MediaProjectionManager) context.getSystemService(Context.MEDIA_PROJECTION_SERVICE);
                        if (mProjectionManager != null) {
                            context.startActivityForResult(mProjectionManager.createScreenCaptureIntent(), REQUEST_PERMISSION_MEDIA_PROJECTION);
                        }
                    }
                })
                .build();
        permissionDialog.show();
    }

    public void onRequestPermissionsResult(@NonNull String permissions, int grantResults){
        onRequestPermissionsResult(0, new String[]{permissions}, new int[]{grantResults});
    }

    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        if (REQUEST_PERMISSION_CODE == requestCode) {
            String permission = permissions[0];
            if (grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                Logger.i(TAG, "current permission granted success-> " + permission);
                requestPermission();
            } else {
                Logger.i(TAG, "current permission denied fail-> " + permission);
                deniedPermission.add(permission);
                requestPermission();
            }
        }
    }

    private PermissionUtils() {
        context = StackManager.getInstance().getMainCurrentActivity();
    }

    public static PermissionUtils getInstance() {
        return Helper.permissionUtils;
    }

    private static final class Helper {
        @SuppressLint("StaticFieldLeak")
        private static final PermissionUtils permissionUtils = new PermissionUtils();
    }

    public interface PermissionsObserver {
        void onPermissionsSuccess();

        void onPermissionsFail();
    }

    public void updateMediaProjection(boolean isPermission){
        this.isMediaProjectionPermission = isPermission;
    }
}