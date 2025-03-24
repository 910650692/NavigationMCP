package com.fy.navi.service.adapter.search.cloud.http;

import android.util.Log;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;

import java.io.IOException;

import okhttp3.Interceptor;
import okhttp3.Request;
import okhttp3.Response;

/**
 * @version \$Revision1.0\$
 * @author baipeng0904
 */
public class SignInterceptor implements Interceptor {
    private static final String TAG = SignInterceptor.class.getSimpleName();

    @NonNull
    @Override
    public Response intercept(final Chain chain) throws IOException {
        Request request = chain.request();
        final Request.Builder requestBuilder = request.newBuilder();
        try {
            // 添加 Header
            requestBuilder.addHeader(HttpConstants.SignHelperKey.CLIENT_ID, "");
            requestBuilder.addHeader(HttpConstants.SignHelperKey.ACCESS_TOKEN, "");
            requestBuilder.addHeader(HttpConstants.SignHelperKey.IDP_USER_ID, "");
            request = requestBuilder.build();
            return chain.proceed(request);
        } catch (IOException e) {
            Logger.d(TAG, Log.getStackTraceString(e));
        }
        return chain.proceed(request);
    }
}
