package com.sgm.navi.patacnetlib.request.navibean.activate;


import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class AppKeyRequest {
    private String mApiVersion;

    public AppKeyRequest(final String apiVersion) {
        this.mApiVersion = apiVersion;
    }
}
