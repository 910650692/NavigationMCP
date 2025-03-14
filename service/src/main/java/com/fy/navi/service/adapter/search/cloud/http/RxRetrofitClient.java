package com.fy.navi.service.adapter.search.cloud.http;


import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

import com.android.utils.log.Logger;

import java.util.concurrent.TimeUnit;

import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import retrofit2.Retrofit;
import retrofit2.adapter.rxjava.RxJavaCallAdapterFactory;
import retrofit2.converter.gson.GsonConverterFactory;

/**
 * <pre>
 *     author : BaiPengMac
 *     e-mail : xxx@xx
 *     time   : 2022/04/13
 *     desc   : OkHttp简单封装
 *     version: xx
 * </pre>
 */
public class RxRetrofitClient {
    private static volatile RxRetrofitClient mInstance;
    private final Retrofit mRetrofit;
    private static final long DEFAULT_CONNECT_TIMEOUT = 15 * 1000;

    public static void init() {
        if (mInstance == null) {
            synchronized (RxRetrofitClient.class) {
                if (mInstance == null) {
                    mInstance = new RxRetrofitClient();
                }
            }
        }
    }

    private RxRetrofitClient() {
        this.mRetrofit = newRetrofit();
    }

    public static <T> T create(final Class<T> service) {
        if (mInstance == null) {
            throw new NullPointerException("未初始化RxRetrofitClient");
        }
        return mInstance.mRetrofit.create(service);
    }

    private Retrofit newRetrofit() {
        Retrofit.Builder builder = new Retrofit.Builder();
        builder.baseUrl(HttpConstants.AppKey_Prod.BASE_CLOUD_URL);
        builder.addConverterFactory(GsonConverterFactory.create());
        builder.addCallAdapterFactory(RxJavaCallAdapterFactory.create());
        builder.client(getOkHttpClient()).build();
        return builder.build();
    }

    private OkHttpClient getOkHttpClient() {
        final HttpLoggingInterceptor loggingInterceptor = new HttpLoggingInterceptor(message -> Logger.e(SEARCH_SERVICE_TAG + "==OKHttp: ", "Message: " + message));
        loggingInterceptor.setLevel(HttpLoggingInterceptor.Level.BODY);

        OkHttpClient.Builder builder = new OkHttpClient.Builder();
        builder.connectTimeout(DEFAULT_CONNECT_TIMEOUT, TimeUnit.MILLISECONDS);
        builder.addInterceptor(loggingInterceptor);
        builder.addInterceptor(new SignInterceptor());
        return builder.build();
    }
}
