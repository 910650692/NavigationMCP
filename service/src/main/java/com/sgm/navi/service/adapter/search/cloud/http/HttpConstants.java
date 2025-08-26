package com.sgm.navi.service.adapter.search.cloud.http;

import androidx.annotation.StringDef;

public class HttpConstants {
    /**
     * 发布环境(对应车型或服务类型)
     */
    @StringDef({
            AppKeyProd.APP_KEY,
            AppKeyProd.APP_SECRET,
            AppKeyProd.BASE_CLOUD_URL,
            AppKeyProd.SERVER_ENV
    })
    public @interface AppKeyProd {
        String SERVER_ENV = "";
        String BASE_CLOUD_URL = "";
        String APP_KEY = "";
        String APP_SECRET = "";
    }

    /**
     * Header参数
     * clientId: B2C接入客户端ID(向IDM申请)
     * accessToken: 客户端授权Token(向IDM申请)
     * idpUserId: 用户唯一标识；可使用Admin用户生成的access token，该入参也可为任意有效用户的idpUserID；(向IDM申请)
     */
    @StringDef({
            SignHelperKey.IDP_USER_ID,
            SignHelperKey.ACCESS_TOKEN,
            SignHelperKey.CLIENT_ID,
    })
    public @interface SignHelperKey {
        String CLIENT_ID = "client_id";
        String ACCESS_TOKEN = "access_token";
        String IDP_USER_ID = "idpUserID";
    }
}
